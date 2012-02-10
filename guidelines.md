# Package guidelines


## Build system specifics:

### Oasis

Oasis support is auto-detected by the presence of of `setup.ml` in the
package's root directory.  An `_oasis` file is not sufficient, as this
would require oasis to be installed (TODO: install and run oasis in
this case?)

All projects packaged with oasis should work out of the box.

odb assumes that the following will work.

```shell
$ ocaml setup.ml -configure [--prefix ~/.odb]
$ ocaml setup.ml -build
$ [sudo] [OCAMLFIND_DESTDIR=~/.odb/lib] ocaml setup.ml -install
```

### OMake

OMake support is auto-detected by the presence of both `OMakefile` and
`OMakeroot`.  The following commands are assumed to be sufficient to build and install such a project:

```shell
$ omake
$ [sudo] [OCAMLFIND_DESTDIR=~/.odb/lib] omake install

```

Note that this currently does not support any `--prefix` option, so if
the package installs by any means other than ocamlfind, it won't be
installed well.

### Makefile

Makefile support is assumed if no other installation method is
available.  This may be changed in the future to check for a
`Makefile` in the project root.  odb detects the make executable by
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
