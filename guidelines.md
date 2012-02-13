# Package guidelines

## General Assumptions

### Installed package detection

`odb` uses very simple heuristics to determine whether a package is
installed.

An `odb` package `foo` that's a library is considered installed if
findlib knows about a library `foo`.  For example, `batteries`
installs itself using findlib with the package name `batteries`.
`odb` knows about this by the `is_library=true` setting in that
package's metadata.

Programs are expected to install (somewhere in the PATH) an executable
named the same as their package name.  For example, `menhir` installs
an executable `menhir`.  `odb` knows about this by the
`is_program=true` setting in package metadata, and uses `where` to
detect this executable.

As long as one of these conditions is satisfied, `odb` will be able to
detect the installation of your package.  Programs that are both
libraries and executables may wish to be marked as both library and
executable, so that if either is missing, `odb` will reinstall the
package.  This is not required, as one or the other may be more
reliably detected. For example, `oasis` installs a findlib package
named `oasis` and installs an executable `oasis`, and is marked as
both `is_library=true` and `is_program=true`.

Programs that do not indicate in their metadata whether they are a
library or a program will be detected as installed if either a library
or executable is found with their name.  This can lead to
mis-identification, for example, for the ocaml library `zip`, it is
important that the presence of the executable `zip` does not cause
`odb` to determine the package is already installed.  Thus it is
recommended to give `odb` this information.

### Tarball structure

When a package is extracted from its tarball, it is expected that
either the tarball contains a single toplevel directory, inside which
is the build infrastructure (make/oasis/omake) or that the tarball
contains the contents of this toplevel directory (this is strongly
discouraged).  No assumptions are made on the name of this toplevel
directory; whatever it's called, `odb` will detect it and use it for
building.  If there are multiple toplevel directories in the tarball,
`odb` will guess one and try to install, but this is not
recommended.

### VCS structure

It is currently assumed that a VCS checkout will get everything in a
release tarball.  If needed, `odb` can be extended with pre-configure
actions for VCS builds, but has no support for generating `configure`
or `setup.ml` at the moment.

## Build systems

It is assumed that native and bytecode compilation will be done when
possible, and only bytecode if native compilation is not available.

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
