#!/usr/bin/env ocaml
#use "topfind";;
#require "str";;
#require "unix";;

(* Configurable parameters, some by command line *)
let webroot = "http://oasis.ocamlcore.org/dev/odb/" 
(*let webroot = "http://mutt.cse.msu.edu:8081/" *)
let odb_home = (Sys.getenv "HOME") ^ "/.odb"
let odb_lib = odb_home ^ "/lib"
let odb_stubs = odb_home ^ "/lib/stublibs"
let odb_bin = odb_home ^ "/bin"
let build_dir = ref odb_home
let cleanup = ref false
let sudo = ref (Unix.geteuid () = 0) (* true if root *)
let to_install = ref []
let force = ref false
let force_all = ref false
let debug = ref false
let repository = ref "testing"
let auto_reinstall = ref false
let reqs = ref [] (* what packages need to be reinstalled because of updates *)

(* micro-stdlib *)
open Printf
open Str
open Arg
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x
let dtap f x = if !debug then f x; x
let (//) x y = if x = "" then y else x
let getenv v = try Sys.getenv v with Not_found -> ""
let mkdir d = if not (Sys.file_exists d) then Unix.mkdir d 0o755

(* Command line argument handling *)
let push_install s = to_install := s :: !to_install
let cmd_line = 
  [ "--clean", Set cleanup, "Cleanup downloaded tarballs and install folders";
    "--sudo", Set sudo, "Switch to root for installs";
    "--force", Set force, "Force (re)installation of packages named";
    "--force-all", Set force_all, "Force (re)installation of dependencies";
    "--debug", Set debug, "Debug package dependencies"; 
    "--repo", Set_string repository, "Set repository [stable, testing, unstable]";
    "--auto-reinstall", Set auto_reinstall, "Auto-reinstall dependent packages on update"
]
    
let () = parse cmd_line push_install "ocaml odb.ml [--sudo] [<packages>]";;

let () = if !repository <> "stable" && !repository <> "testing" && !repository <> "unstable" then (print_endline "Error: Repository must be stable, testing or unstable."; exit 1)

(* micro-http library *)
module Http = struct 
  let get_fn ?(silent=true) uri ~fn =
    let s = if silent then " -s" else "" in
    if Sys.command ("curl -L --url " ^ uri ^ " -o " ^ fn ^ s) <> 0 then 
      failwith ("Curl failed to get " ^ uri)
	
  let get uri =
    if !debug then printf "Getting URI: %s\n%!" uri;
    let fn = Filename.temp_file "odb" ".info" in
    get_fn uri ~fn;
    let ic = open_in fn in
    let len = in_channel_length ic in
    let ret = String.create len in
    really_input ic ret 0 len;
    close_in ic;
    Unix.unlink fn;
    ret
end

(* Type of a package, with its information in a prop list *)
type pkg = {id: string; mutable props: (string * string) list}
type dep_tree = N of pkg * dep_tree list

(* micro property-list library *)
module PL = struct
  let get ~p ~n = try List.assoc n p.props with Not_found -> ""
  let get_b ~p ~n = 
    try List.assoc n p.props |> bool_of_string with Not_found -> false
  let get_i ~p ~n = 
    try List.assoc n p.props |> int_of_string with Not_found -> -1 | Failure "int_of_string" -> failwith (sprintf "Cannot convert %s.%s=\"%s\" to int" p.id n (List.assoc n p.props))

  let of_string = 
    Str.split (Str.regexp "\n") 
    |- List.filter (fun s -> String.contains s '=') 
    |- List.map (fun s -> match Str.bounded_split (Str.regexp " *= *") s 2 with 
	| [k;v] -> (k,v) | [k] -> (k,"") 
	| _ -> failwith ("Bad line in alist: " ^ s))
end

(* locations of files in website *)
let tarball_uri ?(backup=false) p = 
  if backup then
    webroot ^ !repository ^ "/pkg/backup/" ^ (PL.get ~p ~n:"tarball")
  else 
    webroot ^ !repository ^ "/pkg/" ^ (PL.get ~p ~n:"tarball")
let deps_uri id = webroot ^ !repository ^ "/pkg/info/" ^ id

(* wrapper functions to get data from server *)
let get_info id = deps_uri id |> Http.get |> PL.of_string
let get_tarball p =
  let fn = PL.get ~p ~n:"tarball" in
  ( try 
      tarball_uri p |> Http.get_fn ~silent:false ~fn:fn;
    with Failure _ -> 
      tarball_uri ~backup:true p |> Http.get_fn ~silent:false ~fn:fn; );
  fn

(* TODO: verify no bad chars to make command construction safer *)
let to_pkg id = {id=id; props=get_info id} 

(* Dependency comparison library *)  
module Dep = struct
  type cmp = GE | EQ | GT (* Add more?  Add &&, ||? *)
  type ver_comp = Num of int | Str of string
  type ver = ver_comp list
  type ver_req = cmp * ver
  type dep = pkg * ver_req option
  let comp x y = function GE -> x >= y | EQ -> x = y | GT -> x > y

  let rec list_cmp : (ver * ver) -> int = function [],[] -> 0 
    | Num 0::t, [] -> list_cmp (t,[]) | [], Num 0::t -> list_cmp ([],t) 
    | _::_,[] -> 1 | [], _::_ -> -1 
    | (x::xt), (y::yt) when x=y -> list_cmp (xt, yt) 
    | (Num x::_), (Num y::_) -> compare (x:int) y
    | (Str x::_), (Str y::_) -> compare (x:string) y
    | (Num x::_), (Str y::_) -> -1
    | (Str x::_), (Num y::_) -> 1
  let ver_sat req v2 = v2 <> [] && match req with 
    | None -> true 
    | Some (c, v1) -> comp (list_cmp (v1, v2)) 0 c
  let to_ver_comp = function Delim s -> Str s | Text s -> Num (int_of_string s)
  let parse_ver v = 
    try 
      full_split (regexp "[^0-9]+") v |> List.map to_ver_comp
    with Failure _ -> failwith ("Could not parse version: " ^ v)

  let test_lib (p, v) = 
    Sys.command ("ocamlfind query -format %v " ^ p.id ^ " > ocaml-ver") = 0 && 
    open_in "ocaml-ver" |> input_line |> parse_ver |> ver_sat v

  let rec input_all_lines acc ic = 
    let a = try Some (input_line ic) with End_of_file -> None in
    match a with Some w -> input_all_lines (w::acc) ic | None -> List.rev acc

  let get_reqs p =
    let p_id_len = String.length p.id in
    if Sys.command ("ocamlfind query -format %p -d " ^ p.id ^ " > odb-req") = 0 then
      open_in "odb-req" |> input_all_lines [] 
      |> List.filter (fun r -> String.sub r 0 p_id_len <> p.id)
    else []

  let test_prog (p, _v) = Sys.command ("which " ^ p.id) = 0
	
  let has_dep (p,_ as d) = 
    let is_library = PL.get_b p "is_library" in
    let is_program = PL.get_b p "is_program" in
    if is_library || is_program then 
      (is_library && test_lib d) || (is_program && test_prog d)
    else test_lib d || test_prog d;;
  let has_dep (p,v) = has_dep (p,v) |> dtap (fun r -> printf "Package %s dependency satisfied: %B\n%!" p.id r)
  let parse_vreq vr = 
    let l = String.length vr in
    if vr.[0] = '>' && vr.[1] = '=' then (GE, parse_ver (String.sub vr 2 (l-3)))
    else if vr.[0] = '>' then (GT, parse_ver (String.sub vr 1 (l-2)))
    else if vr.[0] = '=' then (EQ, parse_ver (String.sub vr 1 (l-2)))
    else failwith ("Unknown comparator in dependency, cannot parse version requirement: " ^ vr)
  let whitespace_rx = regexp "[ \t]+"
  let make_dep str = 
    let str = global_replace whitespace_rx "" str in
    match bounded_split (regexp_string "(") str 2 with
      | [pkg; vreq] -> to_pkg pkg, Some (parse_vreq vreq)
      | _ -> to_pkg str, None
  let get_deps p = 
    PL.get ~p ~n:"deps" |> Str.split (Str.regexp ",") |> List.map make_dep
  let rec all_deps p = 
    let ds = get_deps p |> List.filter (has_dep |- not) in
    N (p, List.map (fst |- all_deps) ds)
end

let extract_cmd fn = 
  let suff = Filename.check_suffix fn in
  if suff ".tar.gz" || suff ".tgz" then
    "tar -zxvf " ^ fn
  else if suff ".tar.bz2" || suff ".tbz" then
    "tar -jxvf " ^ fn
  else if suff ".tar.xz" || suff ".txz" then
    "tar -Jxvf " ^ fn
  else if suff ".zip" then
    "unzip " ^ fn
  else failwith "Don't know how to extract " ^ fn

let run_or ~cmd ~err = if Sys.command cmd <> 0 then raise err

type build_type = Oasis | Omake | Make

(* Installing a package *)
let install ?(force=false) p = 
  if not force && Dep.has_dep (p,None) then (
    print_endline ("Package " ^ p.id ^ " already installed, use --force to reinstall"); []
  ) else begin
    let install_dir = "install-" ^ p.id in
    if not (Sys.file_exists install_dir) then Unix.mkdir install_dir 0o700;
    Sys.chdir install_dir;
    let tb = get_tarball p in
    let extract_cmd = extract_cmd tb in
    run_or ~cmd:extract_cmd
      ~err:(Failure ("Could not extract tarball for " ^ p.id ^ "(" ^ tb ^ ")"));

    let dirs = (Sys.readdir "." |> Array.to_list |> List.filter Sys.is_directory) in
    (match dirs with [] -> () | h::_ -> Sys.chdir h);

    let buildtype = if Sys.file_exists "setup.ml" then Oasis else if Sys.file_exists "OMakefile" && Sys.file_exists "OMakeroot" then Omake else Make in

    let as_root = PL.get_b p "install_as_root" || !sudo in
    let config_opt = if as_root then "" else " --prefix " ^ odb_home in
    let install_pre = 
      if as_root then "sudo " else 
	"OCAMLFIND_LDCONF=ignore OCAMLFIND_DESTDIR="^odb_lib^" " in

    let config_fail = Failure ("Could not configure " ^ p.id)  in
    let build_fail = Failure ("Could not build " ^ p.id) in
    let install_fail = Failure ("Could not install package " ^ p.id) in

    ( match buildtype with
      | Oasis ->
	run_or ~cmd:("ocaml setup.ml -configure" ^ config_opt) ~err:config_fail;
	run_or ~cmd:"ocaml setup.ml -build" ~err:build_fail;
	run_or ~cmd:(install_pre ^ "ocaml setup.ml -install") ~err:install_fail;
      | Omake ->
	run_or ~cmd:"omake" ~err:build_fail;
	run_or ~cmd:(install_pre ^ "omake install") ~err:install_fail;
      | Make ->
	if Sys.file_exists "configure" then
	  run_or ~cmd:("sh configure" ^ config_opt) ~err:config_fail;
	run_or ~cmd:"make" ~err:build_fail;
	run_or ~cmd:(install_pre ^ "make install") ~err:install_fail;
    );
    Sys.chdir odb_home;
    if not (Dep.has_dep (p,None)) then (
      print_endline ("Problem with installed package: " ^ p.id);
      print_endline ("Installed package is not available to the system");
      print_endline ("Make sure "^odb_bin^" is in your PATH");
      print_endline ("and "^odb_lib^" is in your OCAMLPATH");
      exit 1;
    );
    print_endline ("Successfully installed " ^ p.id);
    Dep.get_reqs p (* return the reqs *)
  end

let install_dep p =
  let rec loop ~force (N (p,deps)) = 
    List.iter (loop ~force:!(force_all)) deps; 
    let rec inner_loop p = 
      let reqs_imm = install ~force p in
      if !auto_reinstall then 
	List.iter 
	  (fun p -> try to_pkg p |> inner_loop with _ -> 
	    reqs := p :: !reqs) 
	  reqs_imm
      else 
	reqs := reqs_imm @ !reqs;
    in
    inner_loop p
  in
  Dep.all_deps p |> loop ~force:(!force || !force_all)

(** MAIN **)
let () = 
  if !sudo then (
    build_dir := getenv "TEMP" // getenv "TMP" // "/tmp"
  ) else (
    mkdir odb_home;
    if not !sudo then (mkdir odb_lib; mkdir odb_bin; mkdir odb_stubs);
  );
  Sys.chdir !build_dir;
  if !cleanup then
    Sys.command ("rm -rf install-*") |> ignore;  
  if !to_install = [] && not !cleanup then ( (* list packages to install *)
    let pkgs = deps_uri "00list" |> Http.get in
    printf "Available packages: %s\n" pkgs
  ) else ( (* install listed packages *)
    List.iter (to_pkg |- install_dep) !to_install;
    if !reqs <> [] then (
      print_endline "Some packages depend on the just installed packages and should be re-installed.";
      print_endline "The command to do this is:";
      print_string "  ocaml odb.ml -force ";
      List.iter (printf "%s ") !reqs;
      print_newline ();
    );
    (* TODO: TEST FOR CAML_LD_LIBRARY_PATH=odb_lib and warn if not set *)
  )
;;
