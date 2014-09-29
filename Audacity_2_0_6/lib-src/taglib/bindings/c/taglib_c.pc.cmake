prefix=${CMAKE_INSTALL_PREFIX}
exec_prefix=${CMAKE_INSTALL_PREFIX}
libdir=${LIB_INSTALL_DIR}
includedir=${INCLUDE_INSTALL_DIR}


Name: TagLib C Bindings
Description: Audio meta-data library (C bindings)
Requires: taglib
Version: ${TAGLIB_LIB_MAJOR_VERSION}.${TAGLIB_LIB_MINOR_VERSION}.${TAGLIB_LIB_PATCH_VERSION}
Libs: -L${LIB_INSTALL_DIR} -ltag_c
Cflags: -I${INCLUDE_INSTALL_DIR}/taglib 
