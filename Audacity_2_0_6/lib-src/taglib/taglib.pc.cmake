prefix=${CMAKE_INSTALL_PREFIX}
exec_prefix=${CMAKE_INSTALL_PREFIX}
libdir=${LIB_INSTALL_DIR}
includedir=${INCLUDE_INSTALL_DIR}

Name: TagLib
Description: Audio meta-data library
Requires: 
Version: ${TAGLIB_LIB_MAJOR_VERSION}.${TAGLIB_LIB_MINOR_VERSION}.${TAGLIB_LIB_PATCH_VERSION}
Libs: -L${LIB_INSTALL_DIR} -ltag
Cflags: -I${INCLUDE_INSTALL_DIR}/taglib 
