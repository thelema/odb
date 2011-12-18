#!/usr/bin/env ocaml
#use "topfind";;
#require "str";;
#require "unix";;
#require "findlib";;

module Fn = Filename

let getenv_def ~def v = try Sys.getenv v with Not_found -> def

(* Configurable parameters, some by command line *)
let webroots =
  Str.split (Str.regexp "|")
    (getenv_def ~def:"http://oasis.ocamlcore.org/dev/odb/" "ODB_PACKAGE_ROOT")
(*let webroots = ["http://mutt.cse.msu.edu:8081/"] *)
let default_base = Fn.concat (Sys.getenv "HOME") ".odb"
let odb_home = getenv_def ~def:default_base "ODB_INSTALL_DIR"
let odb_lib = Fn.concat odb_home "lib"
let odb_stubs = Fn.concat odb_home "/lib/stublibs"
let odb_bin = Fn.concat odb_home "bin"
let build_dir = ref (getenv_def ~def:default_base "ODB_BUILD_DIR")
let cleanup = ref false
let sudo = ref (Unix.geteuid () = 0) (* true if root *)
let to_install = ref []
let force = ref false
let force_all = ref false
let debug = ref false
let repository = ref "testing"
let auto_reinstall = ref false
let have_perms = ref false
let godi = ref (try ignore (Sys.getenv "GODI_LOCALBASE"); true with Not_found -> false)
let configure_flags = ref ""
let configure_flags_global = ref ""
let reqs = ref [] (* what packages need to be reinstalled because of updates *)

(* micro-stdlib *)
open Printf
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x
let dtap f x = if !debug then f x; x
let (//) x y = if x = "" then y else x
let mkdir d = if not (Sys.file_exists d) then Unix.mkdir d 0o755

(* Command line argument handling *)
let push_install s = to_install := s :: !to_install
let cmd_line = Arg.align [
  "--clean", Arg.Set cleanup, " Cleanup downloaded tarballs and install folders";
  "--sudo", Arg.Set sudo, " Switch to root for installs";
  "--have-perms", Arg.Set have_perms, " Don't use --prefix even without sudo";
  "--no-godi", Arg.Clear godi, " Disable use of auto-detected GODI paths";
  "--configure-flags", Arg.Set_string configure_flags, " Flags to pass to explicitly installed packages' configure step";
  "--configure-flags-global", Arg.Set_string configure_flags_global, " Flags to pass to all packages' configure step";
  "--force", Arg.Set force, " Force (re)installation of packages named";
  "--force-all", Arg.Set force_all, " Force (re)installation of dependencies";
  "--debug", Arg.Set debug, " Debug package dependencies";
  "--unstable", Arg.Unit (fun () -> repository := "unstable"), " Use unstable repo";
  "--stable", Arg.Unit (fun () -> repository := "stable"), " Use stable repo";
  "--testing", Arg.Unit (fun () -> repository := "testing"), " Use testing repo [default]";
  "--repo", Arg.Set_string repository, " Set repository [stable, testing, unstable]";
  "--auto-reinstall", Arg.Set auto_reinstall, " Auto-reinstall dependent packages on update";
]

let () =
  Arg.parse cmd_line push_install "ocaml odb.ml [--sudo] [<packages>]";
  if !repository <> "stable" && !repository <> "testing" && !repository <> "unstable" then (print_endline "Error: Repository must be stable, testing or unstable."; exit 1);
  if !godi then print_endline "GODI_LOCALBASE detected, using it for installs";
  ()

(* micro-http library *)
module Http = struct
  let get_fn ?(silent=true) uri ~fn =
    if !debug then printf "Getting URI: %s\n%!" uri;
    let s = if silent then " -s" else "" in
    if Sys.command ("curl -f -k -L --url " ^ uri ^ " -o " ^ fn ^ s) <> 0 then
      failwith ("Curl failed to get " ^ uri)

  let get uri =
    let fn = Fn.temp_file "odb" ".info" in
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
let tarball_uri ?(backup=false) p webroot =
  if backup then
    webroot ^ !repository ^ "/pkg/backup/" ^ (PL.get ~p ~n:"tarball")
  else
    webroot ^ !repository ^ "/pkg/" ^ (PL.get ~p ~n:"tarball")
let deps_uri id webroot = webroot ^ !repository ^ "/pkg/info/" ^ id

(* wrapper functions to get data from server *)
let get_info =
  let ht = Hashtbl.create 10 in
  fun id -> try Hashtbl.find ht id
    with Not_found ->
      try
        List.map (
          fun webroot ->
            try Some (deps_uri id webroot |> Http.get |> PL.of_string |> tap (Hashtbl.add ht id))
            with Failure _ -> None
        ) webroots
        |> List.filter (( <> ) None)
        |> List.map (function Some x -> x | None -> assert false)
        |> List.hd
      with Failure _ -> failwith ("Package not in "^ !repository ^" repo: " ^ id)

let get_tarball p =
  let fn = PL.get ~p ~n:"tarball" in
  try
    ignore (List.find (
      fun webroot ->
        try tarball_uri p webroot |> Http.get_fn ~silent:false ~fn:fn; true
        with Failure _ -> false
    ) webroots);
    fn
  with
  | Not_found ->
      ignore (List.find (
        fun webroot ->
          try tarball_uri ~backup:true p webroot |> Http.get_fn ~silent:false ~fn:fn; true
          with Failure _ -> false
      ) webroots);
      fn

(* TODO: verify no bad chars to make command construction safer *)
let to_pkg id = {id=id; props=get_info id}

(* Version number handling *)
module Ver = struct
  (* A version number is a list of components, with each component
     being a string or a number *)
  type ver_comp = Num of int | Str of string
  type ver = ver_comp list

  (* *)
  let rec cmp : ver -> ver -> int = fun a b -> match a,b with
    | [],[] -> 0 (* each component was equal *)
    | Str"."::Num 0::t, [] -> cmp t [] | [], Str"."::Num 0::t -> cmp [] t (* ignore trailing .0's *)
    | _::_,[] -> 1 | [], _::_ -> -1 (* longer version numbers are before shorter ones *)
    | (x::xt), (y::yt) when x=y -> cmp xt yt (* compare tails when heads are equal *)
    | (Num x::_), (Num y::_) -> compare (x:int) y  (* just compare numbers *)
    | (Str x::_), (Str y::_) -> compare (x:string) y (* extend with name ordering? *)
    | (Num x::_), (Str y::_) -> -1 (* a number is always before a string *)
    | (Str x::_), (Num y::_) -> 1  (* a string is always after a number *)

  let to_ver = function Str.Delim s -> Str s | Str.Text s -> Num (int_of_string s)
  let parse_ver v =
    try Str.full_split (Str.regexp "[^0-9]+") v |> List.map to_ver
    with Failure _ -> failwith ("Could not parse version: " ^ v)

end

(* Dependency comparison library *)
module Dep = struct
  open Ver
  type cmp = GE | EQ | GT (* Add more?  Add &&, ||? *)
  type dep = pkg * (cmp * ver) option
  let comp x y = function GE -> x >= y | EQ -> x = y | GT -> x > y
  let ver_sat req v2 = v2 <> [] && match req with
    | None -> true
    | Some (c, v1) -> comp (Ver.cmp v1 v2) 0 c

  let test_lib (p, v) =
    try
      Findlib.package_property [] p.id "version" |> parse_ver |> ver_sat v
    with Findlib.No_such_package _ ->
      false

  let get_reqs p =
    let p_id_len = String.length p.id in
    try
      Fl_package_base.package_users [] [p.id]
      |> List.filter (fun r -> String.length r < p_id_len || String.sub r 0 p_id_len <> p.id)
    with Findlib.No_such_package _ ->
      []

  let test_prog (p, _v) = Sys.command ("which " ^ p.id ^ "> /dev/null") = 0

  let has_dep (p,_ as d) =
    match (PL.get_b p "is_library",
           PL.get_b p "is_program") with
    | (true,true) | (false,false) -> test_lib d || test_prog d
    | (true,false) -> test_lib d
    | (false,true) -> test_prog d
  let has_dep (p,v) = has_dep (p,v) |> dtap (fun r -> printf "Package %s dependency satisfied: %B\n%!" p.id r)
  let parse_vreq vr =
    let l = String.length vr in
    if vr.[0] = '>' && vr.[1] = '=' then (GE, parse_ver (String.sub vr 2 (l-3)))
    else if vr.[0] = '>' then (GT, parse_ver (String.sub vr 1 (l-2)))
    else if vr.[0] = '=' then (EQ, parse_ver (String.sub vr 1 (l-2)))
    else failwith ("Unknown comparator in dependency, cannot parse version requirement: " ^ vr)
  let whitespace_rx = Str.regexp "[ \t]+"
  let make_dep str =
    let str = Str.global_replace whitespace_rx "" str in
    match Str.bounded_split (Str.regexp_string "(") str 2 with
      | [pkg; vreq] -> to_pkg pkg, Some (parse_vreq vreq)
      | _ -> to_pkg str, None
  let get_deps p =
    PL.get ~p ~n:"deps" |> Str.split (Str.regexp ",") |> List.map make_dep
  let rec all_deps p =
    let ds = get_deps p |> List.filter (has_dep |- not) in
    N (p, List.map (fst |- all_deps) ds)
end

let extract_cmd fn =
  let suff = Fn.check_suffix fn in
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
let install_from_dir ~dir ~force_me p =
  Sys.chdir dir;
    (* detect build type based on files in directory *)
    let buildtype =
           if Sys.file_exists "setup.ml" then Oasis
      else if Sys.file_exists "OMakefile" && Sys.file_exists "OMakeroot" then Omake
      else Make in

    (* configure installation parameters based on command-line flags *)
    let as_root = PL.get_b p "install_as_root" || !sudo in
    let godi_localbase = if !godi then try Sys.getenv "GODI_LOCALBASE" with Not_found -> failwith "$GODI_LOCALBASE must be set if --godi is used" else "" in
    let config_opt = if as_root || !have_perms then "" else if !godi then " --prefix " ^ godi_localbase else " --prefix " ^ odb_home in
    let config_opt = config_opt ^ if List.mem p.id !to_install then (" " ^ !configure_flags) else "" in
    let config_opt = config_opt ^ " " ^ !configure_flags_global in
    let install_pre =
      if as_root then "sudo " else if !have_perms || !godi then "" else
        "OCAMLFIND_LDCONF=ignore OCAMLFIND_DESTDIR="^odb_lib^" " in

    (* define exceptions to raise for errors in various steps *)
    let config_fail = Failure ("Could not configure " ^ p.id)  in
    let build_fail = Failure ("Could not build " ^ p.id) in
    let install_fail = Failure ("Could not install package " ^ p.id) in

    (* Do the install *)
    ( match buildtype with
      | Oasis ->
        run_or ~cmd:("ocaml setup.ml -configure" ^ config_opt) ~err:config_fail;
        run_or ~cmd:"ocaml setup.ml -build" ~err:build_fail;
        (* TODO: MAKE TEST *)
        run_or ~cmd:(install_pre ^ "ocaml setup.ml -install") ~err:install_fail;
      | Omake ->
        run_or ~cmd:"omake" ~err:build_fail;
        (* TODO: MAKE TEST *)
        run_or ~cmd:(install_pre ^ "omake install") ~err:install_fail;
      | Make ->
        if Sys.file_exists "configure" then
          run_or ~cmd:("sh configure" ^ config_opt) ~err:config_fail;
        run_or ~cmd:"make" ~err:build_fail;
        (* TODO: MAKE TEST *)
        run_or ~cmd:(install_pre ^ "make install") ~err:install_fail;
    );
    (* leave the install dir *)
    Sys.chdir !build_dir;
    (* test whether installation was successful *)
    if not (Dep.has_dep (p,None)) then (
      print_endline ("Problem with installed package: " ^ p.id);
      print_endline ("Installed package is not available to the system");
      print_endline ("Make sure "^odb_lib^" is in your OCAMLPATH");
      print_endline ("and "^odb_bin^" is in your PATH");
      exit 1;
    );
    print_endline ("Successfully installed " ^ p.id);
    Dep.get_reqs p (* return the reqs *)

let download_and_install ~force_me p =
  let already_installed = Dep.has_dep (p,None) in
  if not force_me && already_installed then (
    if !force then
      print_endline ("Dependency " ^ p.id ^ " already installed, use --force-all to reinstall")
    else
      print_endline ("Package " ^ p.id ^ " already installed, use --force to reinstall");
    []
  ) else (
    if force_me && already_installed && (PL.get_b p "is_library" || not (PL.get_b p "is_program")) then (
      let as_root = PL.get_b p "install_as_root" || !sudo in
      let install_pre =
        if as_root then "sudo " else if !have_perms || !godi then "" else
            "OCAMLFIND_LDCONF=ignore OCAMLFIND_DESTDIR="^odb_lib^" " in
      print_endline ("Uninstalling forced library " ^ p.id);
      Sys.command (install_pre ^ "ocamlfind remove " ^ p.id) |> ignore;
    );

    (* Set up the directory to install into *)
    let install_dir = "install-" ^ p.id in
    if Sys.file_exists install_dir then
      Sys.command ("rm -rf " ^ install_dir) |> ignore;
    Unix.mkdir install_dir 0o700;

    (* Extract our tarball in that directory *)
    Sys.chdir install_dir;
    let tb = get_tarball p in
    let extract_cmd = extract_cmd tb in
    run_or ~cmd:extract_cmd
      ~err:(Failure ("Could not extract tarball for " ^ p.id ^ "(" ^ tb ^ ")"));

    (* detect and enter directory created by tarball *)
    let dirs = (Sys.readdir "." |> Array.to_list |> List.filter Sys.is_directory) in
    let dir = match dirs with | [] -> "." | h::_ -> h in
    install_from_dir ~dir ~force_me p
  )

(* install a package and all its deps *)
let install_dep p =
  printf "Installing %s\n" p.id;
  let rec install_tree ~force (N (p,deps)) =
    (* take care of child deps first *)
    List.iter (install_tree ~force:!(force_all)) deps;
    let rec inner_loop p =
      let reqs_imm = download_and_install ~force_me:force p in
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
  Dep.all_deps p |> install_tree ~force:(!force || !force_all)

(** MAIN **)
let () =

  (* initialize build directory if needed *)
  if !sudo then (
    build_dir := Fn.temp_dir_name
  ) else (
    mkdir odb_home;
    if not !sudo then (mkdir odb_lib; mkdir odb_bin; mkdir odb_stubs);
  );
  Sys.chdir !build_dir;
  if !cleanup then
    Sys.command ("rm -rf install-*") |> ignore;
  if !to_install = [] && not !cleanup then ( (* list packages to install *)
    let pkgs =
      List.map (
        fun wr ->
          try deps_uri "00list" wr |> Http.get
          with Failure _ -> printf "%s is unavailable or not a valid repository\n\n" wr; ""
      ) webroots
      |> String.concat " "
    in
    let pkgs = Str.split (Str.regexp " +") pkgs in
    match pkgs with
    | [] ->
        print_endline "No packages available"
    | hd :: tl ->
        (* Remove duplicate entries (inefficiently) *)
        let pkgs = List.fold_left (fun accu p -> if List.mem p accu then accu else p :: accu) [hd] tl in
        print_string "Available packages:";
        List.iter (printf " %s") (List.rev pkgs);
        print_newline ();
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
