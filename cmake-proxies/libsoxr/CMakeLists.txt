#directory cmake-proxies/libsoxr
set( TARGET libsoxr )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}libsoxr/examples/1-single-block.c
#${LIB_SRC_DIRECTORY}libsoxr/examples/1a-lsr.c
#${LIB_SRC_DIRECTORY}libsoxr/examples/2-stream.C
#${LIB_SRC_DIRECTORY}libsoxr/examples/3-options-input-fn.c
#${LIB_SRC_DIRECTORY}libsoxr/examples/4-split-channels.c
#${LIB_SRC_DIRECTORY}libsoxr/examples/5-variable-rate.c
#${LIB_SRC_DIRECTORY}libsoxr/src/avfft32.c
#${LIB_SRC_DIRECTORY}libsoxr/src/avfft32s.c
#${LIB_SRC_DIRECTORY}libsoxr/src/cr-core.c
${LIB_SRC_DIRECTORY}libsoxr/src/cr.c
${LIB_SRC_DIRECTORY}libsoxr/src/cr32.c
${LIB_SRC_DIRECTORY}libsoxr/src/cr32s.c
${LIB_SRC_DIRECTORY}libsoxr/src/cr64.c
#${LIB_SRC_DIRECTORY}libsoxr/src/cr64s.c
${LIB_SRC_DIRECTORY}libsoxr/src/data-io.c
${LIB_SRC_DIRECTORY}libsoxr/src/dbesi0.c
${LIB_SRC_DIRECTORY}libsoxr/src/fft4g.c
${LIB_SRC_DIRECTORY}libsoxr/src/fft4g32.c
#${LIB_SRC_DIRECTORY}libsoxr/src/fft4g32s.c
${LIB_SRC_DIRECTORY}libsoxr/src/fft4g64.c
${LIB_SRC_DIRECTORY}libsoxr/src/filter.c
#${LIB_SRC_DIRECTORY}libsoxr/src/pffft-wrap.c
#${LIB_SRC_DIRECTORY}libsoxr/src/pffft.c
#${LIB_SRC_DIRECTORY}libsoxr/src/pffft32.c
${LIB_SRC_DIRECTORY}libsoxr/src/pffft32s.c
#${LIB_SRC_DIRECTORY}libsoxr/src/pffft64s.c
#${LIB_SRC_DIRECTORY}libsoxr/src/soxr-lsr.c
${LIB_SRC_DIRECTORY}libsoxr/src/soxr.c
#${LIB_SRC_DIRECTORY}libsoxr/src/util-simd.c
${LIB_SRC_DIRECTORY}libsoxr/src/util32s.c
#${LIB_SRC_DIRECTORY}libsoxr/src/util64s.c
#${LIB_SRC_DIRECTORY}libsoxr/src/vr-coefs.c
${LIB_SRC_DIRECTORY}libsoxr/src/vr32.c
#${LIB_SRC_DIRECTORY}libsoxr/tests/1-delay-clear.c
#${LIB_SRC_DIRECTORY}libsoxr/tests/throughput.c
#${LIB_SRC_DIRECTORY}libsoxr/tests/vector-cmp.c
#${LIB_SRC_DIRECTORY}libsoxr/tests/vector-gen.c
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_USE_MATH_DEFINES
-D_CRT_SECURE_NO_WARNINGS
-DSOXR_LIB
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/msvc
)

target_link_libraries( ${TARGET} )