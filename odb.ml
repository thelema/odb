#!/usr/bin/env ocaml
#use "topfind";;
#require "str";;
#require "unix";;

let webroot = "http://mutt.cse.msu.edu:8081/"
let odb_home = (Sys.getenv "HOME") ^ "/.odb"
let odb_lib = odb_home ^ "/lib"
let build_dir = ref odb_home
let cleanup = ref false
let sudo = ref (Unix.geteuid () = 0)
let to_install = ref []
let force = ref false
let force_all = ref false
let debug = ref false

open Printf
open Str
open Arg
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x
let dtap f x = if !debug then f x; x
let (//) x y = if x = "" then y else x

let push_install s = to_install := s :: !to_install
let cmd_line = [
  "--clean", Set cleanup, 
  "Cleanup downloaded tarballs and install folders";
  "--sudo", Set sudo,
  "Switch to root for installs";
  "--force", Set force,
  "Force (re)installation of packages named";
  "--force-all", Set force_all,
  "Force (re)installation of dependencies";
  "--debug", Set debug,
  "Debug package dependencies";
]
		
let () = parse cmd_line push_install "ocaml odb.ml [-sudo] <packages>";;

let http_get_fn ?(silent=true) uri ~fn =
  let s = if silent then " -s" else "" in
  if Sys.command ("curl --url " ^ uri ^ " -o " ^ fn ^ s) <> 0 then 
    failwith ("Curl failed to get " ^ uri)

let http_get uri =
  let fn = Filename.temp_file "odb" ".info" in
  http_get_fn uri ~fn;
  let ic = open_in fn in
  let len = in_channel_length ic in
  let ret = String.create len in
  really_input ic ret 0 len;
  close_in ic;
  Unix.unlink fn;
  ret

type pkg = {id: string; mutable props: (string * string) list}
type dep_tree = N of pkg * dep_tree list

let get_prop ~p ~n = try List.assoc n p.props with Not_found -> ""
let get_prop_b ~p ~n = try List.assoc n p.props |> bool_of_string with Not_found -> false
let get_prop_i ~p ~n = try List.assoc n p.props |> int_of_string with Not_found -> -1
let tarball_uri p = webroot ^ "pkg/" ^ (get_prop ~p ~n:"tarball")
let deps_uri id = webroot ^ "pkg/info/" ^ id


let to_alist = Str.split (Str.regexp "\n") |- List.filter (fun s -> String.contains s '=') |- List.map (fun s -> match Str.bounded_split (Str.regexp " *= *") s 2 with [k;v] -> (k,v) | _ -> failwith ("Bad line in alist: " ^ s))
let get_info id = deps_uri id |> http_get |> to_alist

(* TODO: verify no bad chars to make command construction safer *)
let to_pkg id = {id=id; props=get_info id} 

let get_tarball p =
  let fn = Filename.temp_file "odb" ".tgz" in
  tarball_uri p |> http_get_fn ~silent:false ~fn:fn;
  fn
  
type cmp = GE | EQ | GT (* Add more?  Add &&, ||? *)
type ver_req = cmp * int list
type dep = pkg * ver_req option
let comp x y = function GE -> x >= y | EQ -> x = y | GT -> x > y

let rec list_cmp = function [],[] -> 0 | 0::t, [] -> list_cmp (t,[]) | [], 0::t -> list_cmp ([],t) | _::_,[] -> 1 | [], _::_ -> -1 | (x::xt), (y::yt) when x=y -> list_cmp (xt, yt) | (x::_), (y::_) -> compare (x:int) y
let ver_sat req v2 = v2 <> [] && match req with 
  | None -> true 
  | Some (c, v1) -> comp (list_cmp (v1, v2)) 0 c
let parse_ver v = split (Str.regexp_string ".") v |> List.map int_of_string

let has_dep (p,ver_req) = 
  let is_library = get_prop_b p "is_library" in
  let is_program = get_prop_b p "is_program" in
  let test_lib () = 
    Sys.command ("ocamlfind query -format %v" ^ p.id ^ " > ocaml-ver") = 0 && 
      open_in "ocaml-ver" |> input_line |> parse_ver |> ver_sat ver_req
  in
  let test_prog () = Sys.command ("which " ^ p.id) = 0 in
  if is_library || is_program then 
    (is_library && test_lib ()) || (is_program && test_prog ())
  else test_lib () || test_prog ();;
let has_dep (p,v as x) = has_dep (p,v) |> dtap (fun r -> printf "Package %s dependency satisfied: %B\n%!" p.id r)
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
let get_deps p = get_prop ~p ~n:"deps" |> Str.split (Str.regexp ",") |> List.map make_dep
let rec all_deps p = 
  let ds = get_deps p |> List.filter (has_dep |- not) in
  N (p, List.map (fst |- all_deps) ds)

let run_or ~cmd ~err = if Sys.command cmd <> 0 then raise err

let install ?(force=false) p = 
  if not force && has_dep (p,None) then (
    print_endline ("Package " ^ p.id ^ " already installed, use --force to reinstall");
  ) else begin
    let install_dir = "install-" ^ p.id in
    if not (Sys.file_exists install_dir) then Unix.mkdir install_dir 0o700;
    Sys.chdir install_dir;
    let tb = get_tarball p in
    run_or ~cmd:("tar -zxvf " ^ tb) 
      ~err:(Failure ("Could not extract tarball for " ^ p.id));

    let dirs = (Sys.readdir "." |> Array.to_list |> List.filter Sys.is_directory) in
    (match dirs with [] -> () | h::_ -> Sys.chdir h);

    let as_root = get_prop_b p "install_as_root" || !sudo in
    let config_opt = if as_root then "" else " --prefix " ^ odb_home in
    let install_pre = 
      if as_root then "sudo " else "OCAMLFIND_DESTDIR="^odb_lib^" " in

    let config_fail = Failure ("Could not configure " ^ p.id)  in
    let build_fail = Failure ("Could not build " ^ p.id) in
    let install_fail = Failure ("Could not install package " ^ p.id) in

    if Sys.file_exists "setup.ml" then begin (* OASIS BUILD *)
      run_or ~cmd:("ocaml setup.ml -configure" ^ config_opt) ~err:config_fail;
      run_or ~cmd:"ocaml setup.ml -build" ~err:build_fail;
      run_or ~cmd:(install_pre ^ "ocaml setup.ml -install") ~err:install_fail;
    end else if Sys.file_exists "OMakefile" then begin
      run_or ~cmd:"omake" ~err:build_fail;
      run_or ~cmd:(install_pre ^ "omake install") ~err:install_fail;
    end else (* try [./configure &&] make && make install *) begin
      if Sys.file_exists "configure" then
	run_or ~cmd:("sh configure" ^ config_opt) ~err:config_fail;
      run_or ~cmd:"make" ~err:build_fail;
      run_or ~cmd:(install_pre ^ "make install") ~err:install_fail;
    end;
    Sys.chdir odb_home;
    if not (has_dep (p,None)) then (
      print_endline ("Problem with installed package: " ^ p.id);
      print_endline ("Installed package is not available to the system");
      print_endline ("Make sure "^odb_home^"/bin is in your PATH");
      print_endline ("and "^odb_lib^" is in your OCAMLPATH");
      exit 0;
    ) else
      print_endline ("Successfully installed " ^ p.id);
  end

let install_dep p =
  let rec loop ~force (N (p,deps)) = 
    List.iter (loop ~force:!(force_all)) deps; 
    install ~force p 
  in
  all_deps p |> loop ~force:(!force || !force_all)

let pkg_rx = Str.regexp "<a href=.[-a-zA-Z0-9]+.>\\([-a-zA-Z0-9]+\\)</a>"
let get_pkg str = 
  if Str.string_match pkg_rx str 0 then Str.matched_group 1 str else failwith"bad html"
let cleanup_list str =
  Str.split (Str.regexp "<td class=\"n\">") str |> List.tl |> List.tl |> List.map get_pkg |> String.concat " "

let () = 
  if !sudo then (
    build_dir := Sys.getenv "TEMP" // Sys.getenv "TMP" // "/tmp"
  ) else (
    if not (Sys.file_exists odb_home) then Unix.mkdir odb_home 0o755;
    if not (Sys.file_exists odb_lib) && not !sudo then Unix.mkdir odb_lib 0o755;
  );
  Sys.chdir !build_dir;
  if !cleanup then
    Sys.command ("rm -rf install-*") |> ignore;  
  if !to_install = [] && not !cleanup then ( (* list packages to install *)
    print_string "Available packages: ";
    deps_uri "" |> http_get |> cleanup_list |> print_endline
  ) else (* install listed packages *)
    List.iter (to_pkg |- install_dep) !to_install
;;
