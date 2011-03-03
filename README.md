odb: a simple package installer for ocaml

Odb aims to be 80% solution for automated ocaml package installation.

### Setup

Packages will be installed by default to `~/.odb/lib`, so add this
entry to your `/etc/ocamlfind.conf` path or set the environment
variable `OCAMLPATH=~/.odb/lib`.

The latest version can be downloaded directly from github with the following command: 
    curl -O https://github.com/thelema/odb/raw/master/odb.ml


### Usage

Using odb is very easy. 

    ocaml odb.ml

  Prints a list of packages the server knows about

    ocaml odb.ml <packagenames>

  Installs the packages listed (including dependencies)

That's it so far.  Stay tuned for more. (todo: more packages on server)


### Requirements
* [OCaml][] >= 3.11
* [Findlib][] >= 1.2.5
* [curl][]

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[curl]: http://curl.haxx.se/

