# bash file to source for basic odb support

: ${ODB_BASE_DIR=$HOME/.odb}

export PATH=$ODB_BASE_DIR/bin:$PATH
export OCAMLPATH=$ODB_BASE_DIR/lib:$OCAMLPATH
export CAML_LD_LIBRARY_PATH=$ODB_BASE_DIR/lib/stublibs:$CAML_LD_LIBRARY_PATH
