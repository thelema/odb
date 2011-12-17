# bash file to source for basic odb support

: ${ODB_BUILD_DIR=$HOME/.odb}
: ${ODB_INSTALL_DIR=$HOME/.odb}

export PATH=$ODB_INSTALL_DIR/bin:$PATH
export OCAMLPATH=$ODB_INSTALL_DIR/lib:$OCAMLPATH
export CAML_LD_LIBRARY_PATH=$ODB_INSTALL_DIR/lib/stublibs:$CAML_LD_LIBRARY_PATH
