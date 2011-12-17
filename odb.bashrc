# bash file to source for basic odb support
# Set any custom ODB_* environment variables before sourcing this file

: ${ODB_BUILD_DIR=$HOME/.odb}
: ${ODB_INSTALL_DIR=$HOME/.odb}
: ${ODB_PACKAGE_ROOT="http://oasis.ocamlcore.org/dev/odb/"}

export PATH=$ODB_INSTALL_DIR/bin:$PATH
export OCAMLPATH=$ODB_INSTALL_DIR/lib:$OCAMLPATH
export CAML_LD_LIBRARY_PATH=$ODB_INSTALL_DIR/lib/stublibs:$CAML_LD_LIBRARY_PATH
