#directory cmake-proxies/lv2
set( TARGET lv2 )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}lv2/sord/src/sordmm_test.cpp
#${LIB_SRC_DIRECTORY}lv2/suil/src/gtk2_in_qt4.cpp
#${LIB_SRC_DIRECTORY}lv2/suil/src/qt4_in_gtk2.cpp
#${LIB_SRC_DIRECTORY}lv2/suil/src/win_in_gtk2.cpp
#${LIB_SRC_DIRECTORY}lv2/suil/src/x11_in_qt4.cpp

#${LIB_SRC_DIRECTORY}lv2/lilv/bindings/test/bindings_test_plugin.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/collections.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/instance.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/lib.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/node.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/plugin.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/pluginclass.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/port.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/query.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/scalepoint.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/state.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/ui.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/util.c
${LIB_SRC_DIRECTORY}lv2/lilv/src/world.c
#${LIB_SRC_DIRECTORY}lv2/lilv/src/zix/tree.c #see sord source.

#${LIB_SRC_DIRECTORY}lv2/lilv/test/lilv_test.c
#${LIB_SRC_DIRECTORY}lv2/lilv/test/test_plugin.c
#${LIB_SRC_DIRECTORY}lv2/lilv/utils/lilv-bench.c
#${LIB_SRC_DIRECTORY}lv2/lilv/utils/lv2bench.c
#${LIB_SRC_DIRECTORY}lv2/lilv/utils/lv2info.c
#${LIB_SRC_DIRECTORY}lv2/lilv/utils/lv2ls.c
#${LIB_SRC_DIRECTORY}lv2/lv2/lv2/lv2plug.in/ns/ext/atom/atom-test.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-amp.lv2/amp.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-fifths.lv2/fifths.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-metro.lv2/metro.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-midigate.lv2/midigate.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-sampler.lv2/sampler.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-sampler.lv2/sampler_ui.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-scope.lv2/examploscope.c
#${LIB_SRC_DIRECTORY}lv2/lv2/plugins/eg-scope.lv2/examploscope_ui.c
${LIB_SRC_DIRECTORY}lv2/serd/src/env.c
#${LIB_SRC_DIRECTORY}lv2/serd/src/node.c
${LIB_SRC_DIRECTORY}lv2/serd/src/reader.c
#${LIB_SRC_DIRECTORY}lv2/serd/src/serdi.c
${LIB_SRC_DIRECTORY}lv2/serd/src/serdnode.c
${LIB_SRC_DIRECTORY}lv2/serd/src/string.c
${LIB_SRC_DIRECTORY}lv2/serd/src/uri.c
${LIB_SRC_DIRECTORY}lv2/serd/src/writer.c
#${LIB_SRC_DIRECTORY}lv2/serd/tests/serd_test.c
${LIB_SRC_DIRECTORY}lv2/sord/src/sord.c
#${LIB_SRC_DIRECTORY}lv2/sord/src/sordi.c
#${LIB_SRC_DIRECTORY}lv2/sord/src/sord_test.c
#${LIB_SRC_DIRECTORY}lv2/sord/src/sord_validate.c
${LIB_SRC_DIRECTORY}lv2/sord/src/syntax.c
${LIB_SRC_DIRECTORY}lv2/sord/src/zix/digest.c
${LIB_SRC_DIRECTORY}lv2/sord/src/zix/hash.c
${LIB_SRC_DIRECTORY}lv2/sord/src/zix/tree.c
${LIB_SRC_DIRECTORY}lv2/sratom/src/sratom.c
#${LIB_SRC_DIRECTORY}lv2/sratom/tests/sratom_test.c
${LIB_SRC_DIRECTORY}lv2/suil/src/host.c
#${LIB_SRC_DIRECTORY}lv2/suil/src/instance.c
${LIB_SRC_DIRECTORY}lv2/suil/src/suil_instance.c
#${LIB_SRC_DIRECTORY}lv2/suil/src/x11_in_gtk2.c

)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
-Dsnprintf=_snprintf
-Dinline=__inline # Not needed in non CMake version.
-DHAVE_FMAX
-DLILV_INTERNAL
-D_DEBUG
 )
add_library( ${TARGET} STATIC ${SOURCES})
add_compile_options(/TP)
target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/windows
${TARGET_SOURCE}/sord/src
${TARGET_SOURCE}/lilv
${TARGET_SOURCE}/lv2
${TARGET_SOURCE}/serd
${TARGET_SOURCE}/sord
${TARGET_SOURCE}/sratom
${TARGET_SOURCE}/suil
)

target_link_libraries( ${TARGET} )