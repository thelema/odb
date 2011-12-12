odb: a simple package installer for ocaml

Odb aims to be an 80% solution for automated ocaml package installation.

### Setup

Packages will be installed by default to `~/.odb/lib`, so add this
entry to your `/etc/findlib.conf` path or set the environment
variable `OCAMLPATH=~/.odb/lib`.

The latest version can be downloaded directly from github with the following command: 
    
    curl -O https://raw.github.com/thelema/odb/master/odb.ml


### Usage

Using odb is very easy. To print a list of available packages, do:

    ocaml odb.ml

Once you've chosen some packages, the following command will install them (and their dependencies):

    ocaml odb.ml <packagenames>

If you install a package that uses C stub libraries, you will need to add `$HOME/.odb/lib/stublibs` to your `ocaml/ld.conf` file.  The following command does this:

    echo $HOME/.odb/lib/stublibs | sudo tee -a `ocamlc -where`/ld.conf




### Requirements
* [OCaml][] >= 3.11
* [Findlib][] >= 1.2.5
* [curl][]

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[curl]: http://curl.haxx.se/

