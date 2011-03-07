#!/usr/bin/env ocaml
#use "topfind";;
#require "str";;
#require "unix";;

let webroot = "http://mutt.cse.msu.edu:8081/"
let odb_home = (Sys.getenv "HOME") ^ "/.odb"
let odb_lib = odb_home ^ "/lib"
let cleanup = ref false
let sudo = ref false
let to_install = ref []

open Printf
open Str
open Arg
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x

let push_install s = to_install := s :: !to_install
let cmd_line = [
  "--clean", Set cleanup, 
  "Cleanup downloaded tarballs and install folders";
  "--sudo", Set sudo,
  "Switch to root for installs";
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
  
let has_dep p = 
  let is_library = get_prop_b p "is_library" in
  let is_program = get_prop_b p "is_program" in
  if is_library || is_program then 
    (is_library && Sys.command ("ocamlfind query " ^ p.id) = 0) 
    || (is_program && Sys.command ("which " ^ p.id) = 0)
  else
    Sys.command ("ocamlfind query " ^ p.id) = 0 
    || Sys.command ("which " ^ p.id) = 0

let get_deps p = get_prop ~p ~n:"deps" |> Str.split (Str.regexp ",") |> List.map to_pkg
let rec all_deps p = 
  let ds = get_deps p |> List.filter (has_dep |- not) in
  N (p, List.map all_deps ds)

let run_or ~cmd ~err = if Sys.command cmd <> 0 then failwith err

let install ?(force=false) p = 
  if not force && has_dep p then () else
  begin
    let install_dir = "install-" ^ p.id in
    if not (Sys.file_exists install_dir) then Unix.mkdir install_dir 0o700;
    Sys.chdir install_dir;
    let tb = get_tarball p in
    run_or ~cmd:("tar -zxvf " ^ tb) 
      ~err:("Could not extract tarball for " ^ p.id);

    let dirs = (Sys.readdir "." |> Array.to_list |> List.filter Sys.is_directory) in
    (match dirs with [] -> () | h::_ -> Sys.chdir h);

    let as_root = get_prop_b p "install_as_root" || !sudo in
    let config_opt = if as_root then "" else " --prefix " ^ odb_home in
    let install_pre = 
      if as_root then "sudo " else "OCAMLFIND_DESTDIR="^odb_lib^" " in

    let config_fail = ("Could not configure " ^ p.id)  in
    let build_fail = ("Could not build " ^ p.id) in
    let install_fail = ("Could not install package " ^ p.id) in

    if Sys.file_exists "setup.ml" then begin (* OASIS BUILD *)
      run_or ~cmd:("ocaml setup.ml -configure" ^ config_opt) ~err:config_fail;
      run_or ~cmd:"ocaml setup.ml -build" ~err:build_fail;
      run_or ~cmd:(install_pre ^ "ocaml setup.ml -install") ~err:install_fail;
    end else if Sys.file_exists "OMakefile" then begin
      run_or ~cmd:"omake" ~err:build_fail;
      run_or ~cmd:(install_pre ^ "omake install") ~err:install_fail;
    end else begin
      if Sys.file_exists "configure" then
	run_or ~cmd:("sh configure" ^ config_opt) ~err:config_fail;
      run_or ~cmd:"make" ~err:build_fail;
      run_or ~cmd:(install_pre ^ "make install") ~err:install_fail;
    end;
    Sys.chdir odb_home;
    if not (has_dep p) then (
      print_endline ("Failure installing package: " ^ p.id ^ " - installed package is not available to the system");
      print_endline ("Either add "^odb_home^"/bin to your PATH or");
      print_endline ("add "^odb_lib^" to your OCAMLPATH");
      failwith "Exiting"
    ) else
      print_endline ("Successfully installed " ^ p.id);
  end

let install_dep p =
  let rec loop ~force (N (p,deps)) = 
    List.iter (loop ~force:false) deps; 
    install ~force p 
  in
  all_deps p |> loop ~force:true

let pkg_rx = Str.regexp "<a href=.[-a-zA-Z0-9]+.>\\([-a-zA-Z0-9]+\\)</a>"
let get_pkg str = 
  if Str.string_match pkg_rx str 0 then Str.matched_group 1 str else failwith"bad html"
let cleanup_list str =
  Str.split (Str.regexp "<td class=\"n\">") str |> List.tl |> List.tl |> List.map get_pkg |> String.concat " "

let () = 
  if not (Sys.file_exists odb_home) then Unix.mkdir odb_home 0o755;
  if not (Sys.file_exists odb_lib) then Unix.mkdir odb_lib 0o755;
  Sys.chdir odb_home;
  if !cleanup then
    Sys.command ("rm -rf install*") |> ignore;  
  if !to_install = [] && not !cleanup then ( (* list packages to install *)
    print_string "Available packages: ";
    deps_uri "" |> http_get |> cleanup_list |> print_endline
  ) else (* install listed packages *)
    List.iter (to_pkg |- install_dep) !to_install
;;
