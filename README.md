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

### Local Packages

Odb has recently added support for local packages.  This means it can
auto-install packages from sources other than oasis-db.  At the moment, these packages cannot have dependencies that odb will know about, support for this is planned in future releases.

To inform odb about a package that exists in a local directory, put a
line like one of the following lines into your `~/.odb/packages` file:

    dep batteries local-dir /home/thelema/batteries

For a package that's available as a tarball from a http source,

    dep foo remote-tar-gz http://www.ocamlforge.org/directory/to/tarball-ver.tgz

These packages will override any packages of the same name avaiable at oasis-db.

### Requirements
* [OCaml][] >= 3.11
* [Findlib][] >= 1.2.5
* [curl][]

[Findlib]: http://projects.camlcity.org/projects/findlib.html/
[OCaml]: http://caml.inria.fr/ocaml/release.en.html
[curl]: http://curl.haxx.se/
