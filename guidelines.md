# Package guidelines

## General Assumptions

### Installed package detection

`odb` uses very simple heuristics to determine whether a package is
installed.  Packages can be libraries or programs (or both).
Libraries are expected to install themselves using findlib using a
findlib name that's the same as their package name.  For example,
`batteries` installs itself using findlib with the package name
`batteries`.  Programs are expected to install (somewhere in the PATH)
an executable named the same as their package name.  For example,
`menhir` installs an executable `menhir`.

Programs that are both should do both.  For example, `oasis` installs
a findlib package named `oasis` and installs an executable `oasis`.
Programs that are not tagged as either should do one or the other (and
should then be properly tagged so detection is more accurate).

### Tarball structure

When a package is extracted from its tarball, it is expected that
either the tarball contains a single toplevel directory, inside which
is the build infrastructure (make/oasis/omake) or that the tarball
contains the contents of this toplevel directory.  No assumptions are
made on the name of this toplevel directory; whatever it's called, `odb`
will detect it and use it for building.  If there are multiple
toplevel directories in the tarball, batteries will guess one and try
to install, but this is not recommended.

### VCS structure

It is currently assumed that a VCS checkout will get everything in a
release tarball.  If needed, `odb` can be extended with pre-configure
actions for VCS builds, but has no support for generating `configure`
or `setup.ml` at the moment.

## Build system specifics:

### Oasis

Oasis support is auto-detected by the presence of of `setup.ml` in the
package's root directory.  An `_oasis` file is not currently
sufficient, as this would require oasis to be installed.  (`odb` TODO:
install and run oasis in this case)

All projects packaged with oasis should work out of the box.

`odb` assumes that the following will work.

```shell
$ ocaml setup.ml -configure [--prefix ~/.odb]
$ ocaml setup.ml -build
$ [sudo] [OCAMLFIND_DESTDIR=~/.odb/lib] ocaml setup.ml -install
```

### OMake

OMake support is auto-detected by the presence of both `OMakefile` and
`OMakeroot`.  The following commands are assumed to be sufficient to
build and install such a project:

```shell
$ omake
$ [sudo] [OCAMLFIND_DESTDIR=~/.odb/lib] omake install

```

Note that this currently does not support any `--prefix` option, so if
the package installs by any means other than ocamlfind, it won't be
installed well.

`odb` will fail if omake is not installed.

### Makefile

Makefile support is assumed if no other installation method is
available.  This may be changed in the future to check for a
`Makefile` in the project root.  `odb` detects the make executable by
probing for `gnumake`, `gmake` and `make` in that order.  The commands
shown here will assume that `make` was detected.

If a `configure` file is present in the package root directory, the
following will be run:

```shell
$ ./configure [--prefix ~/.odb]
```

Then make is invoked as follows:

```shell
$ make
$ [sudo] [OCAMLFIND_DESTDIR=~/.odb/lib] make install
```
