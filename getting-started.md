This document will cover a basic build of [OCaml][], [findlib][], and a base set
of libraries and tools from their original source code.  There are other methods
available for installing [OCaml][], including [GODI][] and your Linux
distribution's native packages.  Whichever approach you take, please read all of
the available documentation.  This document is only a summary - it will be
useful and likely necessary to know more than is presented here.

A bash shell and basic development toolchain is assumed, along with [OCaml][]
version 3.12.1 and [findlib][] version 1.2.7.

# Building OCaml and findlib From Source

## Building OCaml

1. Define the location of your OCaml installation as an environment variable for
   simplicity while following these steps.
    * `export OCAML_BASE=$HOME/ocaml`
    * `mkdir -p $OCAML_BASE/build` -- Where we will do our initial build
1. Download and extract [OCaml 3.12.1][OCaml 3.12.1 tarball]
    * `cd $OCAML_BASE/build`
    * `curl -O http://caml.inria.fr/pub/distrib/ocaml-3.12/ocaml-3.12.1.tar.gz`
    * `tar xzf ocaml-3.12.1.tar.gz`
1. Build [OCaml][]
    * `cd ocaml-3.12.1`
    * `./configure -prefix $OCAML_BASE`
    * `make world.opt` -- There is currently no support for a parallel build of
      OCaml itself.  This step may take a while.
1. Install [OCaml][]
    * `make install`
1. Install the compiler libraries for [OCaml][]
    * `mkdir -p $OCAML_BASE/lib/ocaml/compiler-libs/typing`
    * `mkdir -p $OCAML_BASE/lib/ocaml/compiler-libs/parsing`
    * `mkdir -p $OCAML_BASE/lib/ocaml/compiler-libs/utils`
    * `cp typing/*.cmi $OCAML_BASE/lib/ocaml/compiler-libs/typing/`
    * `cp parsing/*.cmi $OCAML_BASE/lib/ocaml/compiler-libs/parsing/`
    * `cp utils/*.cmi $OCAML_BASE/lib/ocaml/compiler-libs/utils/`
1. Setup your environment to find your freshly installed [OCaml][]
    * `export PATH=$OCAML_BASE/bin:$PATH`

## Building findlib

1. From the same terminal, download and extract
   [findlib 1.2.7][findlib 1.2.7 tarball]
    * `cd $OCAML_BASE/build` -- You should already be here
    * `curl -O http://download.camlcity.org/download/findlib-1.2.7.tar.gz`
    * `tar xzf findlib-1.2.7.tar.gz`
1. Build [findlib][]
    * `cd findlib-1.2.7`
    * `./configure` -- This should automatically configure [findlib][] to
      install alongside [OCaml][]
    * `make all opt`
1. Install [findlib][]
    * `make install`
1. Configure [findlib][]
    * ``mkdir `ocamlfind printconf destdir`/stublibs``
    * ``echo `ocamlfind printconf destdir`/stublibs >> `ocamlfind printconf ldconf` ``

You should, at this point, have a working installation of [OCaml][] and
[findlib][].  You may want to add the following lines to the end of your
`.bashrc` so that these tools are available from your `$PATH` in all future
terminal sessions:

    export OCAML_BASE=$HOME/ocaml
    export PATH=$OCAML_BASE/bin:$PATH

# Getting odb, oasis and Batteries

[odb][] is a simple, easy to use client for the [oasis-db][] [OCaml][] package
repository.

[oasis][] is a tool for creating build systems for [OCaml][] programs and
libraries.

[Batteries][] is a library for [OCaml][] which extends OCaml's standard library
with many useful additions.

## Fetching and setting up odb

1. From a terminal, download [odb.ml][odb.ml github]
    * `cd $OCAML_BASE/bin`
    * `curl -O https://raw.github.com/thelema/odb/master/odb.ml`
    * `chmod +x odb.ml`
1. Setup the environment for [odb][]
    * `cd $OCAML_BASE/etc`
    * `curl -O https://raw.github.com/thelema/odb/master/odb.bashrc`
    * `source odb.bashrc`

You want want to add the following line to the end of your `.bashrc` so that
the environment will be setup the same way every time you open a new terminal:

    source $OCAML_BASE/etc/odb.bashrc

## Installing oasis

1. Let [odb][] do the work, including installing several dependencies
    * `odb.ml oasis`

## Installing Batteries

1. Once again, [odb][] handles the heavy lifting
    * `odb.ml batteries`

# Example project using oasis and Batteries

To Be Written

# Other goodies

## utop, an enhanced toplevel/interactive environment

1. Install [React][], an optional dependency of [Lwt][]
    * `odb.ml react`
1. Install [Lwt][] with [React][] support enabled (required by [utop][])
    * `odb.ml --configure-flags --enable-react lwt`
1. Install [utop][] and the remaining packages it depends on
    * `odb.ml utop`

[GODI]: http://godi.camlcity.org/godi/index.html
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[findlib]: http://projects.camlcity.org/projects/findlib.html
[odb]: https://github.com/thelema/odb
[oasis]: http://oasis.forge.ocamlcore.org/
[Batteries]: http://batteries.forge.ocamlcore.org/
[React]: http://erratique.ch/software/react
[Lwt]: http://ocsigen.org/lwt/
[utop]: http://forge.ocamlcore.org/projects/utop/

[OCaml 3.12.1 tarball]: http://caml.inria.fr/pub/distrib/ocaml-3.12/ocaml-3.12.1.tar.gz
[findlib 1.2.7 tarball]: http://download.camlcity.org/download/findlib-1.2.7.tar.gz
[odb.ml github]: https://raw.github.com/thelema/odb/master/odb.ml

