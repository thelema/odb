odb: a simple package installer for ocaml

Odb aims to be an 80% solution for automated ocaml package installation.

### Setup

Packages will be installed by default to `~/.odb/lib`, so add this
entry to your `/etc/findlib.conf` (or `/etc/ocamlfind.conf`) path or set the environment
variable `OCAMLPATH=~/.odb/lib`.

The latest version can be downloaded directly from github with the following command:

    curl -O https://raw.github.com/thelema/odb/master/odb.ml


### Usage

Using odb is very easy. To print a list of available packages, do:

    ocaml odb.ml

Once you've chosen some packages, the following command will install
them (and their dependencies):

    ocaml odb.ml <packagenames>

If you install a package that uses C stub libraries, you will need to
add `$HOME/.odb/lib/stublibs` to your `ocaml/ld.conf` file.  The
following command does this:

    echo $HOME/.odb/lib/stublibs | sudo tee -a `ocamlc -where`/ld.conf

If you don't have write access to your `ld.conf` file, you can instead
set the environment variable:

    export CAML_LD_LIBRARY_PATH=$HOME/.odb/lib/stublibs

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

Odb also supports package metadata from a local `packages` file.To
inform odb about a package that exists in a local directory, put a
line like one of the following lines into your `~/.odb/packages` file:

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