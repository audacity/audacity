#directory cmake-proxies/expat
set( TARGET expat )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}expat/amiga/expat_68k.c
#${LIB_SRC_DIRECTORY}expat/amiga/expat_68k_handler_stubs.c
#${LIB_SRC_DIRECTORY}expat/amiga/expat_lib.c
#${LIB_SRC_DIRECTORY}expat/amiga/expat_vectors.c
#${LIB_SRC_DIRECTORY}expat/amiga/launch.c
#${LIB_SRC_DIRECTORY}expat/examples/elements.c
#${LIB_SRC_DIRECTORY}expat/examples/outline.c

${LIB_SRC_DIRECTORY}expat/lib/xmlparse.c
${LIB_SRC_DIRECTORY}expat/lib/xmlrole.c
${LIB_SRC_DIRECTORY}expat/lib/xmltok.c
${LIB_SRC_DIRECTORY}expat/lib/xmltok_impl.c
${LIB_SRC_DIRECTORY}expat/lib/xmltok_ns.c

#${LIB_SRC_DIRECTORY}expat/tests/chardata.c
#${LIB_SRC_DIRECTORY}expat/tests/minicheck.c
#${LIB_SRC_DIRECTORY}expat/tests/runtests.c
#${LIB_SRC_DIRECTORY}expat/tests/benchmark/benchmark.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/codepage.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/ct.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/readfilemap.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/unixfilemap.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/win32filemap.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/xmlfile.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/xmlmime.c
#${LIB_SRC_DIRECTORY}expat/xmlwf/xmlwf.c

#${LIB_SRC_DIRECTORY}expat/tests/runtestspp.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB 
-DCOMPILED_FROM_DSP
)
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/lib
)

target_link_libraries( ${TARGET} )