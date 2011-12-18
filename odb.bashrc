# bash file to source for basic odb support
# Set any custom ODB_* environment variables before sourcing this file

# Each package will be downloaded to and built under this directory
: ${ODB_BUILD_DIR=$HOME/.odb}
# Each package will be install under this directory
: ${ODB_INSTALL_DIR=$HOME/.odb}
# A list of available repositories, separated by '|' (vertical pipe)
: ${ODB_PACKAGE_ROOT="http://oasis.ocamlcore.org/dev/odb/"}

# Make sure odb-installed binaries and libraries are ready to use with findlib
export PATH=$ODB_INSTALL_DIR/bin:$PATH
export OCAMLPATH=$ODB_INSTALL_DIR/lib:$OCAMLPATH
export CAML_LD_LIBRARY_PATH=$ODB_INSTALL_DIR/lib/stublibs:$CAML_LD_LIBRARY_PATH
