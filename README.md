odb: a simple package installer for ocaml

Odb aims to be an 80% solution for automated ocaml package installation.

### Setup

Packages will be installed by default to `~/.odb/lib`, so add this
entry to your `/etc/findlib.conf` (or `/etc/ocamlfind.conf`) path or
set the environment variable `OCAMLPATH=~/.odb/lib`.

If you install a package that uses C stub libraries, you will need to
add `$HOME/.odb/lib/stublibs` to your `ocaml/ld.conf` file.  The
following command does this:

    echo $HOME/.odb/lib/stublibs | sudo tee -a `ocamlc -where`/ld.conf

If you don't have write access to your `ld.conf` file, you can instead
set the environment variable:

    export CAML_LD_LIBRARY_PATH=$HOME/.odb/lib/stublibs

The latest version of odb can be downloaded directly from github with
the following command:

    curl -O https://raw.github.com/thelema/odb/master/odb.ml


### Usage

Using odb is very easy. To print a list of available packages, do:

    ocaml odb.ml

Once you've chosen some packages, the following command will install
them (and their dependencies):

    ocaml odb.ml <packagenames>

You can also install packages by giving odb tarballs (local or remote):

    ocaml odb.ml http://prdownloads.sourceforge.net/camomile/camomile-0.8.3.tar.bz2

Or git URLs:

    ocaml odb.ml https://github.com/thelema/bench.git

Or local source directories:

    ocaml odb.ml /home/thelema/camomile-0.8.2/

When not installing by a plain package name, odb will try to infer
dependencies, but is currently unable to identify the package name to
see if it's already installed.  To fix this, use a package file, as
described in the "Local Packages" section below.


### Oasis-DB

Odb supports installing packages uploaded to Oasis-DB.  These packages
are grouped into three "repositories", Stable, Testing and Unstable.
The list of packages and versions of each are shown on the Oasis-DB
website here: http://oasis.ocamlcore.org/dev/odb/

The latest version of any package on Oasis-DB is available from the
Unstable Repository.  No checking is done on these packages by the
repository maintainers, so use at your own risk.  Use `--unstable` to
enable use of this repository.

After a package is verified as at least minimally working by an
Oasis-DB admin, it is upgraded to the Testing repository.  This
repository is where pckages go for testing.  Use `--testing` to use
the testing repository.

The default repository is the Stable repository, where packages have
been better verified to work with each other.  These packages should
be free of version conflicts, and should all work together.  To
explicitly use the stable repository, use `--stable`, although this is
the default if no other repository is selected.

It is possible to override the use of Oasis-DB and provide your own
server with package metadata by setting the `ODB_PACKAGE_ROOT`
environment variable.

### Local Packages

Odb also supports package metadata from a local `packages` file. Odb
reads these files on startup to find out about packages that aren't
available on Oasis-DB or to override the data on Oasis-DB.  There are
two `packages` files that odb will read; one in `~/.odb/packages` and
the other in the same directory as the `odb.ml` file.

For installation from a local source directory, add the line

    batteries dir=/home/thelema/batteries

For a package that's available as a tarball from a http source,

    foo tarball=http://www.ocamlforge.org/directory/to/foo-ver.tgz

These packages will override any packages of the same name avaiable at
oasis-db.  Further documentation of the file format is given in the
example `packages` file in the odb source tree.

### Requirements
* [OCaml][] >= 3.12
* [Findlib][] >= 1.2.5
* [curl][] or [wget][]

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[curl]: http://curl.haxx.se/
[wget]: http://www.gnu.org/software/wget/


### Environment Variables

Odb will pay attention to a number of environment variables for
configuration purposes:

ODB_PACKAGE_ROOT: defaults to "http://oasis.ocamlcore.org/dev/odb/"
  Set this to the root URL of your remote repository

ODB_INSTALL_DIR: defaults to $HOME/.odb
  Base directory for odb installation; changing this affects LIB/STUBS/BIN

ODB_LIB_DIR: defaults to $HOME/.odb/lib or $ODB_INSTALL_DIR/lib
  Where findlib libraries are installed; needs to be in $OCAML_PATH

ODB_STUBS_DIR: defaults to $HOME/.odb/stublibs or $ODB_INSTALL_DIR/stublibs
  Where stub libraries are installed; needs to be in `ld.conf`

ODB_BIN_DIR: defaults to $HOME/.odb/bin or $ODB_INSTALL_DIR/bin
  Where executables are installed; needs to be in $PATH

ODB_BUILD_DIR: defaults to $HOME/.odb/build or $ODB_INSTALL_DIR/build
  Where downloaded tarballs are extracted to for building

GODI_LOCALBASE / OCAML_BASE:
  If set, then value will be used for `--prefix` and OCAMLFIND_DESTDIR
  will not be set when installing (as ocamlfind should already be
  setup to install to correct location)
