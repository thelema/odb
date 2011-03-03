odb: a simple package installer for ocaml

Odb aims to be 80% solution for automated ocaml package installation.

Packages will be installed by default to `~/.odb/lib`, so add it to
your `/etc/ocamlfind.conf` path

Usage:
    `ocaml odb.ml`
  Prints a list of packages the server knows about
    `ocaml odb.ml <packagenames>`
  Installs the packages listed (including dependencies)

That's it so far.  Stay tuned for more.


### Requirements
* [OCaml][] >= 3.11
* [Findlib][] >= 1.2.5
* [curl][]

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[curl]: http://curl.haxx.se/

