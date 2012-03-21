#!/usr/bin/env ocaml
(* Permission is granted to use and modify this program under WTFPL *)
#use "topfind";;
#require "str";;
#require "unix";;
#require "findlib";;

(* micro-stdlib *)
module Fn = Filename
let (</>) = Fn.concat
open Printf
let (|>) x f = f x
let (|-) f g x = g (f x)
let tap f x = f x; x
let debug = ref false
let dtap f x = if !debug then f x; x
let (//) x y = if x = "" then y else x
let iff p f x = if p x then f x else x
let mkdir d = if not (Sys.file_exists d) then Unix.mkdir d 0o755
let getenv_def ~def v = try Sys.getenv v with Not_found -> def
let indir d f = let l=Sys.getcwd () in printf "indir %s -> %s\n%!" l d; Sys.chdir d; let r=f() in Sys.chdir l; printf "outdir %s -> %s\n%!" d l; r
let detect_exe exe = Sys.command ("which " ^ exe ^ " > /dev/null") = 0
let get_exe () = (* returns the full path and name of the current program *)
  Sys.argv.(0) |> iff Fn.is_relative (fun e -> Sys.getcwd () </> e)
  |> iff (fun e -> Unix.((lstat e).st_kind = S_LNK)) Unix.readlink
let run_or ~cmd ~err = if Sys.command cmd <> 0 then raise err

(* Configurable parameters, some by command line *)
let webroots =
  Str.split (Str.regexp "|")
    (getenv_def ~def:"http://oasis.ocamlcore.org/dev/odb/" "ODB_PACKAGE_ROOT")
(*let webroots = ["http://mutt.cse.msu.edu:8081/"] *)
let default_base = (Sys.getenv "HOME") </> ".odb"
let odb_home = getenv_def ~def:default_base "ODB_INSTALL_DIR"
let odb_lib = odb_home </> "lib"
let odb_stubs = odb_home </> "/lib/stublibs"
let odb_bin = odb_home </> "bin"
let build_dir = ref (getenv_def ~def:default_base "ODB_BUILD_DIR")
let cleanup = ref false
let sudo = ref (Unix.geteuid () = 0) (* true if root *)
let to_install = ref []
let force = ref false
let force_all = ref false
let repository = ref "stable"
let auto_reinstall = ref false
let have_perms = ref false (* auto-detected in main *)
let ignore_unknown = ref false
let get_only = ref false
let print_info = ref false
let godi = ref (try ignore (Sys.getenv "GODI_LOCALBASE"); true with Not_found -> false)
let configure_flags = ref ""
let configure_flags_global = ref ""
let reqs = ref [] (* what packages need to be reinstalled because of updates *)


(* Command line argument handling *)
let push_install s = to_install := s :: !to_install
let cmd_line = Arg.align [
  "--clean", Arg.Set cleanup, " Cleanup downloaded tarballs and install folders";
  "--sudo", Arg.Set sudo, " Switch to root for installs";
  "--have-perms", Arg.Set have_perms, " Don't use --prefix even without sudo";
  "--no-godi", Arg.Clear godi, " Disable use of auto-detected GODI paths";
  "--configure-flags", Arg.Set_string configure_flags, " Flags to pass to explicitly installed packages' configure step";
  "--configure-flags-all", Arg.Set_string configure_flags_global, " Flags to pass to all packages' configure step";
  "--force", Arg.Set force, " Force (re)installation of packages named";
  "--force-all", Arg.Set force_all, " Force (re)installation of dependencies";
  "--debug", Arg.Set debug, " Debug package dependencies";
  "--unstable", Arg.Unit (fun () -> repository := "unstable"), " Use unstable repo";
  "--stable", Arg.Unit (fun () -> repository := "stable"), " Use stable repo";
  "--testing", Arg.Unit (fun () -> repository := "testing"), " Use testing repo [default]";
  "--auto-reinstall", Arg.Set auto_reinstall, " Auto-reinstall dependent packages on update";
  "--ignore", Arg.Set ignore_unknown, " Don't fail on unknown package name";
  "--get", Arg.Set get_only, " Only download and extract packages; don't install";
  "--info", Arg.Set print_info, " Only print the metadata for the packages listed; don't install";
]

let () =
  Arg.parse cmd_line push_install "ocaml odb.ml [--sudo] [<packages>]";
  if !godi then print_endline "GODI_LOCALBASE detected, using it for installs";
  ()

(* micro-http library *)
module Http = struct
  let dl_cmd =
    if detect_exe "curl" then (fun ~silent uri fn ->
      let s = if silent then " -s" else "" in
      "curl -f -k -L --url " ^ uri ^ " -o " ^ fn ^ s)
    else if detect_exe "wget" then (fun ~silent uri fn ->
      let s = if silent then " -q" else "" in
      "wget --no-check-certificate " ^ uri ^ " -O " ^ fn ^ s)
    else (fun ~silent:_ _uri _fn ->
      failwith "neither curl nor wget was found, cannot download")
  let get_fn ?(silent=true) uri ?(fn=Fn.basename uri) () =
    if !debug then printf "Getting URI: %s\n%!" uri;
    if Sys.command (dl_cmd ~silent uri fn) <> 0 then
      failwith ("failed to get " ^ uri);
    fn
  let get_contents uri =
    let fn = Fn.temp_file "odb" ".info" in
    ignore(get_fn uri ~fn ());
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

(* micro property-list library *)
module PL = struct
  let get ~p ~n = try List.assoc n p.props with Not_found -> ""
  let get_opt ~p ~n = try Some (List.assoc n p.props) with Not_found -> None
  let get_b ~p ~n =
    try List.assoc n p.props |> bool_of_string with Not_found -> false
  let get_i ~p ~n =
    try List.assoc n p.props |> int_of_string with Not_found -> -1 | Failure "int_of_string" -> failwith (sprintf "Cannot convert %s.%s=\"%s\" to int" p.id n (List.assoc n p.props))

  let split_pair s = match Str.bounded_split (Str.regexp " *= *") s 2 with
    | [k;v] -> (k,v) | [k] -> (k,"")
    | _ -> failwith ("Bad line in alist: " ^ s)
  let of_string =
    Str.split (Str.regexp "\n")
    |- List.filter (fun s -> String.contains s '=') |- List.map split_pair
  let add ~p k v = p.props <- (k,v) :: p.props
  let modify_assoc ~n f pl = try let old_v = List.assoc n pl in
    (n, f old_v) :: List.remove_assoc n pl with Not_found -> pl
  let has_key ~p k0 = List.mem_assoc k0 p.props
  let print p =
    printf "%s\n" p.id; List.iter (fun (k,v) -> printf "%s=%s\n" k v) p.props
end

let deps_uri id webroot = webroot ^ !repository ^ "/pkg/info/" ^ id
let mod_tarball webroot fn = webroot ^ !repository ^ "/pkg/" ^ fn |> dtap (printf "tarball at %s\n")

(* wrapper functions to get data from server *)
let info_cache = Hashtbl.create 10
let get_info id = (* gets a package's info from the repo *)
  try Hashtbl.find info_cache id with Not_found ->
    let rec find_uri = function
      | [] -> failwith ("Package not in " ^ !repository ^" repo: " ^ id)
      | webroot :: tl ->
        try deps_uri id webroot |> Http.get_contents |> PL.of_string
      |> tap (Hashtbl.add info_cache id)
      (* hack the tarball location so it's prefixed by the server address *)
      |> PL.modify_assoc ~n:"tarball" (mod_tarball webroot)
        with Failure _ -> find_uri tl
    in
    find_uri webroots

let parse_package_file fn =
  if Sys.file_exists fn then
    let ic = open_in fn in let line = ref 0 in
    try while true do incr line; (* TODO: dep foo ver x=y x2=y2...\n *)
        match Str.split (Str.regexp " +") (input_line ic) with
	  | h::_ when h.[0] = '#' -> () (* ignore comments *)
	  | [] -> ()                    (* and blank lines *)
          | ["dep"; id; "remote-tar-gz"; url] ->
            Hashtbl.add info_cache id ["tarball", url]
          | ["dep"; id; "local-dir"; dir] ->
            Hashtbl.add info_cache id ["dir", dir]
          | ["dep"; id; "local-tar-gz"; filename] ->
            Hashtbl.add info_cache id ["tarball", filename]
          | ["dep"; id; "git"; url] ->
            Hashtbl.add info_cache id ["git", url]
	  | id::tl when List.for_all (fun s -> String.contains s '=') tl ->
	    Hashtbl.add info_cache id (List.map PL.split_pair tl)
          | _ -> printf "W: packages file %s line %d is invalid\n" fn !line
      done; assert false
    with End_of_file -> printf "%d packages loaded from %s\n" (Hashtbl.length info_cache) fn

let get_tarball p = (* returns a local filename for the given tarball *)
  let tb = (PL.get ~p ~n:"tarball") in
  if String.sub tb 0 5 = "http:" || String.sub tb 0 4 = "ftp:" then
    (* download to current dir *)
    Http.get_fn ~silent:false tb ()
  else tb (* assume is a local file already *)

let get_tarball_chk p = (* checks md5 if possible *)
  let fn = get_tarball p in
  let sum = Digest.file fn |> Digest.to_hex in
  if PL.has_key ~p "md5" then
    if sum <> (PL.get ~p ~n:"md5") then
      (eprintf "Tarball %s failed md5sum verification, aborting\n" fn; exit 5)
    else printf "Tarball %s passed md5sum check\n" fn
  else printf "Tarball %s has no md5 in package info\nmd5sum: %s\n" fn sum;
  fn

(* TODO: verify no bad chars to make command construction safer *)
let to_pkg id = (* TODO: AUTODETECT URLs AND PATHS *)
  (* if id.[0] = '/' then { id =  *)
  {id = id; props = get_info id}

(* Version number handling *)
module Ver = struct
  (* A version number is a list of (string or number) *)
  type ver_comp = Num of int | Str of string
  type ver = ver_comp list

  let rec cmp : ver -> ver -> int = fun a b -> match a,b with
    | [],[] -> 0 (* each component was equal *)
    (* ignore trailing .0's *)
    | Str"."::Num 0::t, [] -> cmp t [] | [], Str"."::Num 0::t -> cmp [] t
    (* longer version numbers are before shorter ones *)
    | _::_,[] -> 1 | [], _::_ -> -1
    (* compare tails when heads are equal *)
    | (x::xt), (y::yt) when x=y -> cmp xt yt
    (* just compare numbers *)
    | (Num x::_), (Num y::_) -> compare (x:int) y
    (* extend with name ordering? *)
    | (Str x::_), (Str y::_) -> compare (x:string) y
    | (Num x::_), (Str y::_) -> -1 (* a number is always before a string *)
    | (Str x::_), (Num y::_) -> 1  (* a string is always after a number *)

  let to_ver = function
    | Str.Delim s -> Str s
    | Str.Text s -> Num (int_of_string s)
  let parse_ver v =
    try Str.full_split (Str.regexp "[^0-9]+") v |> List.map to_ver
    with Failure _ -> failwith ("Could not parse version: " ^ v)

  let comp_to_string = function Str s -> s | Num n -> string_of_int n
  let to_string v = List.map comp_to_string v |> String.concat ""
end

(* Dependency comparison library *)
module Dep = struct
  open Ver
  type cmp = GE | EQ | GT (* Add more?  Add &&, ||? *)
  type dep = pkg * (cmp * ver) option
  let comp vc = function GE -> vc >= 0 | EQ -> vc = 0 | GT -> vc > 0
  let comp_to_string = function GE -> ">=" | EQ -> "=" | GT -> ">"
  let ver_sat ~req = function None -> false | Some ver -> match req with
    | None -> true
    | Some (c, vreq) -> comp (Ver.cmp ver vreq) c
  let req_to_string = function None -> ""
    | Some (c,ver) -> (comp_to_string c) ^ (Ver.to_string ver)

  let get_reqs p =
    let p_id_len = String.length p.id in
    try
      Fl_package_base.package_users [] [p.id]
      |> List.filter (fun r -> String.length r < p_id_len
	                    || String.sub r 0 p_id_len <> p.id)
    with Findlib.No_such_package _ ->
      []
  let installed_ver_lib p =
    try Some (Findlib.package_property [] p.id "version" |> parse_ver)
    with Findlib.No_such_package _ -> None
  let installed_ver_prog p =
    if Sys.command ("which \"" ^ p.id ^ "\" > /dev/null") <> 0 then None
    else
      try
        let fn = Fn.temp_file "odb" ".ver" in
        ignore(Sys.command (p.id ^ " --version > " ^ fn ^ " 2> /dev/null"));
        let ic = open_in fn in
        let ver_string = input_line ic in
        close_in ic;
        Sys.remove fn;
        Some (parse_ver ver_string)
      with _ -> Some [] (* unknown ver *)
  let get_ver p =
    match (PL.get_b p "is_library",
           PL.get_b p "is_program") with
    | (true,true) | (false,false) ->
      (match installed_ver_lib p with None -> installed_ver_prog p | x -> x)
    | (true,false) -> installed_ver_lib p
    | (false,true) -> installed_ver_prog p
  let has_dep (p,req) =
    get_ver p |> ver_sat ~req
    |> dtap (fun r -> printf "Package %s(%s) dependency satisfied: %B\n%!" p.id (req_to_string req) r)
  let parse_vreq vr =
    let l = String.length vr in
    if vr.[0] = '>' && vr.[1] = '=' then (GE, parse_ver (String.sub vr 2 (l-3)))
    else if vr.[0] = '>' then (GT, parse_ver (String.sub vr 1 (l-2)))
    else if vr.[0] = '=' then (EQ, parse_ver (String.sub vr 1 (l-2)))
    else failwith ("Unknown comparator in dependency, cannot parse version requirement: " ^ vr)
  let whitespace_rx = Str.regexp "[ \t]+"
  let make_dep str =
    try
      let str = Str.global_replace whitespace_rx "" str in
      match Str.bounded_split (Str.regexp_string "(") str 2 with
        | [pkg; vreq] -> to_pkg pkg, Some (parse_vreq vreq)
        | _ -> to_pkg str, None
    with x -> if !ignore_unknown then {id="";props=[]}, None else raise x
  let string_to_deps s = Str.split (Str.regexp ",") s |> List.map make_dep
      |> List.filter (fun (p,_) -> p.id <> "")
  let get_deps p = PL.get ~p ~n:"deps" |> string_to_deps
  let of_oasis dir = (* reads the [dir]/_oasis file *)
    let fn = dir </> "_oasis" in
    if not (Sys.file_exists fn) then [] else
      let ic = open_in fn in
      let deps = ref [] in
      try while true do
	  let line = input_line ic in
	  match Str.bounded_split (Str.regexp_string ":") line 2 with
	    | [("BuildDepends"|"  BuildDepends"); ds] -> (* FIXME, fragile *)
	      deps := (string_to_deps ds) @ !deps;
	    | _ -> ()
	done; assert false
      with End_of_file -> close_in ic; !deps
end

let extract_cmd fn = (* TODO?: check for gzip/bzip2/etc *)
  let suff = Fn.check_suffix fn in
  if suff ".tar.gz" || suff ".tgz" then       "tar -zxf " ^ fn
  else if suff ".tar.bz2" || suff ".tbz" then "tar -jxf " ^ fn
  else if suff ".tar.xz" || suff ".txz" then  "tar -Jxf " ^ fn
  else if suff ".zip" then                    "unzip " ^ fn
  else failwith ("Don't know how to extract " ^ fn)

type build_type = Oasis | Omake | Make

(* Installing a package *)
let install_from_current_dir p =
  if !debug then printf "Installing %s from %s" p.id (Sys.getcwd ());
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
        "OCAMLFIND_DESTDIR="^odb_lib^" " in

  (* define exceptions to raise for errors in various steps *)
  let config_fail = Failure ("Could not configure " ^ p.id)  in
  let build_fail = Failure ("Could not build " ^ p.id) in
  (*    let test_fail = Failure ("Tests for package " ^ p.id ^ "did not complete successfully") in*)
  let install_fail = Failure ("Could not install package " ^ p.id) in

  (* Do the install *)
  ( match buildtype with
    | Oasis ->
      run_or ~cmd:("ocaml setup.ml -configure" ^ config_opt) ~err:config_fail;
      run_or ~cmd:"ocaml setup.ml -build" ~err:build_fail;
      (*        run_or ~cmd:"ocaml setup.ml -test" ~err:test_fail;*)
      run_or ~cmd:(install_pre ^ "ocaml setup.ml -install") ~err:install_fail;
    | Omake ->
      if not (detect_exe "omake") then
        failwith "OMake executable not found; cannot build";
      run_or ~cmd:"omake" ~err:build_fail;
      (* TODO: MAKE TEST *)
      run_or ~cmd:(install_pre ^ "omake install") ~err:install_fail;
    | Make ->
      if Sys.file_exists "configure" then
        run_or ~cmd:("sh configure" ^ config_opt) ~err:config_fail;
      (* Autodetect 'gnumake', 'gmake' and 'make' *)
      let make =
        if detect_exe "gnumake" then "gnumake" else
          if detect_exe "gmake" then "gmake" else
            if detect_exe "make" then "make" else
              failwith "No gnumake/gmake/make executable found; cannot build"
      in
      run_or ~cmd:make ~err:build_fail;
      (* TODO: MAKE TEST *)
      run_or ~cmd:(install_pre ^ make ^ " install") ~err:install_fail;
  );
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

(* detect directory created by tarball extraction or git clone *)
let find_install_dir dir =
  let is_dir fn = Sys.is_directory (dir </> fn) in
  try dir </> (Sys.readdir dir |> Array.to_list |> List.find is_dir)
  with Not_found -> dir

let make_install_dir pid =
  (* Set up the directory to install into *)
  let install_dir = !build_dir </> ("install-" ^ pid) in
  if Sys.file_exists install_dir then
    Sys.command ("rm -rf " ^ install_dir) |> ignore;
  Unix.mkdir install_dir 0o700;
  install_dir

let clone ~cmd act p =
  let idir = make_install_dir p.id in
  let err = Failure ("Could not " ^ act ^ " for " ^ p.id) in
  indir idir (fun () -> run_or ~cmd ~err);
  find_install_dir idir

let extract_tarball p =
  let idir = make_install_dir p.id in
  let err = Failure ("Could not extract tarball for " ^ p.id) in
  indir idir (fun () -> run_or ~cmd:(extract_cmd (get_tarball_chk p)) ~err);
  if !debug then printf "Extracted tarball for %s into %s\n%!" p.id idir;
  find_install_dir idir

let clone_git p = clone ~cmd:("git clone --depth=1 " ^ PL.get p "git" ^ (if PL.get p "branch" <> "" then (" --branch " ^ PL.get p "branch") else "")) "clone git" p
let clone_svn p = clone ~cmd:("svn checkout " ^ PL.get p "svn") "checkout svn" p
let clone_cvs p =
  let idir = make_install_dir p.id in
  run_or ~cmd:("cvs -z3 -d" ^ PL.get p "cvs" ^ " co " ^ PL.get p "cvspath")
    ~err:(Failure ("Could not checkout cvs for " ^ p.id));
  idir </> (PL.get p "cvspath") (* special dir for cvs *)
let clone_hg p = clone ~cmd:("hg clone " ^ PL.get p "hg") "clone mercurial" p
let clone_darcs p = clone ~cmd:("darcs get --lazy " ^ PL.get p "darcs") "get darcs" p

(* returns the directory that the package was extracted to *)
let get_package p =
  if PL.has_key ~p "tarball" then extract_tarball p
  else if PL.has_key ~p "dir" then (PL.get ~p ~n:"dir")
  else if PL.has_key ~p "git" then clone_git p
  else if PL.has_key ~p "svn" then clone_svn p
  else if PL.has_key ~p "cvs" then clone_cvs p
  else if PL.has_key ~p "hg"  then clone_hg p
  else if PL.has_key ~p "darcs" then clone_darcs p
  else if PL.has_key ~p "deps" then "." (* packages that are only deps are ok *)
  else failwith ("No download method available for: " ^ p.id)

let get_package p = get_package p |> tap (printf "package downloaded to %s\n%!")

let uninstall p =
  let as_root = PL.get_b p "install_as_root" || !sudo in
  let install_pre =
    if as_root then "sudo " else if !have_perms || !godi then "" else
        "OCAMLFIND_DESTDIR="^odb_lib^" " in
  print_endline ("Uninstalling forced library " ^ p.id);
  Sys.command (install_pre ^ "ocamlfind remove " ^ p.id) |> ignore

let install_package p =
  (* uninstall forced libraries *)
  if (Dep.get_ver p) <> None && (PL.get_b p "is_library" || not (PL.get_b p "is_program")) then
    uninstall p;
  indir (get_package p) (fun () -> install_from_current_dir p)

(* install a package and all its deps *)
let rec install_full ?(root=false) p =
  let force_me = !force_all || (root && !force) in
  match Dep.get_ver p with
    (* abort if old version of dependency exists and no --force-all *)
    | Some v when not root && not !force_all ->
      printf "\nDependency %s has version %s but needs to be upgraded.\nTo allow odb to do this, use --force-all\nAborting." p.id (Ver.to_string v);
      exit 1
    (* warn if a dependency is already installed *)
    | Some v when not force_me ->
      let cat, arg = if root then "Package", "--force" else "Dependency", "--force-all" in
      printf "%s %s(%s) already installed, use %s to reinstall\n" cat p.id (Ver.to_string v) arg;
    | _ ->
      printf "Installing %s\n%!" p.id;
      let deps = Dep.get_deps p in
      List.iter (fun (p,_ as d) -> if not (Dep.has_dep d) then install_full p) deps;
      printf "Deps for %s satisfied\n%!" p.id;
      let rec install_get_reqs p =
        let reqs_imm = install_package p in
        if !auto_reinstall then
          List.iter
            (fun pid -> try to_pkg pid |> install_get_reqs with _ ->
              reqs := pid :: !reqs) (* if install of req fails, print pid *)
            reqs_imm
        else
          reqs := reqs_imm @ !reqs; (* unreinstalled reqs *)
      in
      install_get_reqs p

(** MAIN **)
let () = (* Command line arguments already parsed above *)
  parse_package_file (odb_home </> "packages");
  parse_package_file (Fn.dirname (get_exe ()) </> "packages");
(* TEMP DISABLE - TODO: MAKE WORK WITH ocamlbrew
  (* if we have permission to the install dir, set have_perms *)
  (try Unix.(access (Findlib.default_location ()) [R_OK;W_OK;X_OK]); have_perms := true; print_endline "Install permission detected" with Unix.Unix_error _ -> ());
 *)
  (* initialize build directory if needed *)
  if !sudo then build_dir := Fn.temp_dir_name
  else (
    mkdir odb_home;
    if not !sudo then (mkdir odb_lib; mkdir odb_bin; mkdir odb_stubs);
  );
  Sys.chdir !build_dir;
  if !cleanup then ( Sys.command ("rm -rvf install-*") |> ignore; exit 0 );
  if !to_install = [] then ( (* list packages to install *)
    let pkgs =
      List.map (fun wr ->
        try deps_uri "00list" wr |> Http.get_contents
        with Failure _ -> printf "%s is unavailable or not a valid repository\n\n" wr; ""
      ) webroots |> String.concat " "
    in
    let pkgs = Str.split (Str.regexp " +") pkgs in
    (match pkgs with
    | [] -> print_endline "No packages available"
    | hd :: tl -> (* Remove duplicate entries (inefficiently) *)
        let pkgs = List.fold_left (fun accu p -> if List.mem p accu then accu else p :: accu) [hd] tl in
        print_string "Available packages from oasis:";
        List.iter (printf " %s") (List.rev pkgs);
        print_newline ()
    );
    print_string "Locally configured packages:";
    Hashtbl.iter (fun k _v -> printf " %s" k) info_cache;
    print_newline ()
  ) else if !get_only then ( (* just download packages *)
    let print_loc pid =
      printf "Package %s downloaded to %s\n" pid (to_pkg pid |> get_package) in
    List.iter print_loc (List.rev !to_install);
  ) else if !print_info then (
    List.iter (to_pkg |- PL.print) (List.rev !to_install);
  ) else ( (* install listed packages *)
    List.iter (to_pkg |- install_full ~root:true) (List.rev !to_install);
    if !reqs <> [] then (
      print_endline "Some packages depend on the just installed packages and should be re-installed.";
      print_endline "The command to do this is:";
      print_string "  ocaml odb.ml -force ";
      List.iter (printf "%s ") !reqs;
      print_newline ();
    );
    (* TODO: TEST FOR CAML_LD_LIBRARY_PATH=odb_lib and warn if not set *)
  )
