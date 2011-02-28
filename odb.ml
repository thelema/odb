(* ocamlfind ocamlopt -linkpkg -package batteries,netclient -g -w Z odb.ml -o odb *)
let webroot = "http://mutt.cse.msu.edu:8081/"
let odb_home = (Sys.getenv "HOME") ^ "/.odb"
let cleanup = false


open Batteries_uni
open Http_client.Convenience
open Printf
open Str

type pkg = {id: string; mutable props: (string * string) list}
type dep_tree = N of pkg * dep_tree list

let get_prop ~p ~n = try List.assoc n p.props with Not_found -> ""
let get_prop_b ~p ~n = try List.assoc n p.props |> bool_of_string with Not_found -> false
let get_prop_i ~p ~n = try List.assoc n p.props |> int_of_string with Not_found -> -1
let tarball_uri p = webroot ^ "pkg/" ^ (get_prop ~p ~n:"tarball")
let deps_uri id = webroot ^ "pkg/info/" ^ id


let split_by pat str = String.nsplit str pat
let to_alist = split_by "\n" |- List.filter_map 
    (fun s -> if String.contains s '=' then Some (String.split s "=" |> Tuple2.map String.trim) else None)
let get_info id = deps_uri id |> tap (printf "uri:%s\n%!") |> http_get |> to_alist

(* TODO: verify no bad chars to make command construction safer *)
let to_pkg id = {id=id; props=get_info id} 

let get_tarball p = (* TODO: make efficient *)
  let tarball_contents = tarball_uri p |> http_get in
  File.with_temporary_out ~suffix:".tgz" 
    (fun oc fn -> IO.nwrite oc tarball_contents; fn)
  (* returns the filename the tarball is stored in *)
  
let has_dep p = Sys.command ("ocamlfind query " ^ p.id) = 0 || Sys.command ("which " ^ p.id) = 0
let get_deps p = get_prop ~p ~n:"deps" |> split_by "," |> List.map to_pkg
let rec all_deps p = 
  let ds = get_deps p |> List.filter (has_dep |- not) in
  N (p, List.map all_deps ds)

let install p = 
    (*  if true then printf "Install: %s\n" p.id else  *)
  begin
    let install_dir = "install-" ^ p.id in
    if not (Sys.file_exists install_dir) then Unix.mkdir install_dir 0o700;
    Sys.chdir install_dir;
    let tb = get_tarball p in
    if Sys.command ("tar -zxvf " ^ tb) <> 0 then 
      failwith ("Could not extract tarball for " ^ p.id);

    (Sys.files_of "." // Sys.is_directory) |> Enum.get |> Option.may Sys.chdir;

    if Sys.file_exists "setup.ml" then begin (* OASIS BUILD *)
      if Sys.command ("ocaml setup.ml -configure") <> 0 then
	failwith ("Could not configure " ^ p.id);
      if Sys.command ("ocaml setup.ml -build") <> 0 then
	failwith ("Could not build " ^ p.id);
      if Sys.command ("ocaml setup.ml -install") <> 0 then
	failwith ("Could not install package " ^ p.id);
    end else begin
      if Sys.file_exists "configure" then
	Sys.command ("sh configure") |> ignore;
      if Sys.command ("make") <> 0 then
	failwith ("Could not build " ^ p.id);
      if Sys.command ("OCAMLFIND_DESTDIR="^odb_home^"/lib make install") <> 0 then
	failwith ("Could not install package " ^ p.id);
    end;
    Sys.chdir odb_home;
    if cleanup then Sys.command ("rm -rf " ^ install_dir) |> ignore;
  end

let install_dep p =
  let rec loop (N (p,deps)) =
    List.iter loop deps;
    install p
  in
  loop (all_deps p)

let pkg_rx = Str.regexp "<a href=.[-a-zA-Z0-9]+.>\\([-a-zA-Z0-9]+\\)</a>"
let get_pkg str = 
  if Str.string_match pkg_rx str 0 then Str.matched_group 1 str else failwith"bad html"
let cleanup_list str =
  String.nsplit str "<td class=\"n\">" |> List.drop 2 |> List.map get_pkg |> String.concat " "

let () = 
  if not (Sys.file_exists odb_home) then Unix.mkdir odb_home 0o700;
  Sys.chdir odb_home;
  if Array.length Sys.argv = 1 then ( (* list packages to install *)
    print_string "Available packages: ";
    deps_uri "" |> http_get |> cleanup_list |> print_endline
  ) else (* install listed packages *)
    args () |> Enum.iter (to_pkg |- install_dep);
