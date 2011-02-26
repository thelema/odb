(* ocamlfind ocamlopt -linkpkg -package batteries,netclient -g -w Z odb.ml -o odb *)

open Batteries_uni
open Http_client.Convenience
open Printf

type pkg = {id: string; mutable props: (string * string) list}
type dep_tree = N of pkg * dep_tree list

let webroot = "http://mutt.cse.msu.edu/"
let odb_home = (Sys.getenv "HOME") ^ "/.odb"

let get_prop ~p ~n = List.assoc n p.props
let tarball_uri p = webroot ^ "pkg/" ^ (get_prop ~p ~n:"tarball")
let deps_uri id = webroot ^ "pkg/dep/" ^ id


let split_by pat str = String.nsplit str pat
let to_alist = split_by "\n" |- List.map 
    (fun s -> String.split s "=" |> Tuple2.map String.trim)
let get_info id = deps_uri id |> tap (printf "uri:%s\n%!") |> http_get |> to_alist

(* TODO: verify no bad chars to make command construction safer *)
let to_pkg id = {id=id; props=get_info id} 

let get_tarball p = (* TODO: make efficient *)
  let tarball_contents = tarball_uri p |> http_get in
  File.with_temporary_out ~suffix:".tgz" 
    (fun oc fn -> IO.nwrite oc tarball_contents; fn)
  (* returns the filename the tarball is stored in *)
  
let has_dep p = Sys.command ("ocamlfind query " ^ p.id) = 0  
let get_deps p = get_prop ~p ~n:"deps" |> split_by "," |> List.map to_pkg
let rec all_deps p = 
  printf "ad: %s = " p.id;
  let ds = get_deps p |> List.filter (has_dep |- not) in
  printf "%a\n%!" (List.print String.print) (List.map (fun i -> i.id) ds);
  N (p, List.map all_deps ds)

let install p = 
  if true then printf "Install: %s\n" p.id
  else begin
    let install_dir = "install-" ^ p.id in
    Unix.mkdir install_dir 0o700;
    Sys.chdir install_dir;
    let tb = get_tarball p in
    if Sys.command ("tar -zxvf " ^ tb) <> 0 then 
      failwith ("Could not extract tarball for " ^ p.id);
    let tbdir = Sys.files_of "." // Sys.is_directory |> Enum.get |> Option.get in
    Sys.chdir tbdir;
    if Sys.file_exists "configure" then
      Sys.command ("./configure") |> ignore;
    if Sys.command ("make") <> 0 then
      failwith ("Could not build tarball for " ^ p.id);
    if Sys.command ("OCAMLFIND_DESTDIR="^odb_home^"/lib make install") <> 0 then
      failwith ("Could not install package " ^ p.id);
    Sys.chdir "../.."
  end

let install_dep p =
  let rec loop (N (p,deps)) =
    List.iter loop deps;
    install p
  in
  loop (all_deps p)

let () = 
  if not (Sys.file_exists odb_home) then Unix.mkdir odb_home 0o700;
  Sys.chdir odb_home;
  args () |> Enum.iter (to_pkg |- install_dep);
