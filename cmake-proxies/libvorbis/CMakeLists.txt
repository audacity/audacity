#directory cmake-proxies/libvorbis
set( TARGET libvorbis )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}libvorbis/examples/chaining_example.c
#${LIB_SRC_DIRECTORY}libvorbis/examples/decoder_example.c
#${LIB_SRC_DIRECTORY}libvorbis/examples/encoder_example.c
#${LIB_SRC_DIRECTORY}libvorbis/examples/seeking_example.c
#${LIB_SRC_DIRECTORY}libvorbis/examples/vorbisfile_example.c
${LIB_SRC_DIRECTORY}libvorbis/lib/analysis.c
#${LIB_SRC_DIRECTORY}libvorbis/lib/barkmel.c
${LIB_SRC_DIRECTORY}libvorbis/lib/bitrate.c
${LIB_SRC_DIRECTORY}libvorbis/lib/block.c
${LIB_SRC_DIRECTORY}libvorbis/lib/codebook.c
${LIB_SRC_DIRECTORY}libvorbis/lib/envelope.c
${LIB_SRC_DIRECTORY}libvorbis/lib/floor0.c
${LIB_SRC_DIRECTORY}libvorbis/lib/floor1.c
${LIB_SRC_DIRECTORY}libvorbis/lib/info.c
${LIB_SRC_DIRECTORY}libvorbis/lib/lookup.c
${LIB_SRC_DIRECTORY}libvorbis/lib/lpc.c
${LIB_SRC_DIRECTORY}libvorbis/lib/lsp.c
${LIB_SRC_DIRECTORY}libvorbis/lib/mapping0.c
${LIB_SRC_DIRECTORY}libvorbis/lib/mdct.c
${LIB_SRC_DIRECTORY}libvorbis/lib/psy.c
#${LIB_SRC_DIRECTORY}libvorbis/lib/psytune.c
${LIB_SRC_DIRECTORY}libvorbis/lib/registry.c
${LIB_SRC_DIRECTORY}libvorbis/lib/res0.c
${LIB_SRC_DIRECTORY}libvorbis/lib/sharedbook.c
${LIB_SRC_DIRECTORY}libvorbis/lib/smallft.c
${LIB_SRC_DIRECTORY}libvorbis/lib/synthesis.c
#${LIB_SRC_DIRECTORY}libvorbis/lib/tone.c
${LIB_SRC_DIRECTORY}libvorbis/lib/vorbisenc.c
${LIB_SRC_DIRECTORY}libvorbis/lib/vorbisfile.c
${LIB_SRC_DIRECTORY}libvorbis/lib/window.c
#${LIB_SRC_DIRECTORY}libvorbis/macos/compat/strdup.c
#${LIB_SRC_DIRECTORY}libvorbis/test/test.c
#${LIB_SRC_DIRECTORY}libvorbis/test/util.c
#${LIB_SRC_DIRECTORY}libvorbis/test/write_read.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/bookutil.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/build.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/cascade.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/distribution.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/genericdata.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/huffbuild.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/latticebuild.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/latticehint.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/latticepare.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/latticetune.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/lspdata.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/metrics.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/residuedata.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/residuesplit.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/run.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/train.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/vqgen.c
#${LIB_SRC_DIRECTORY}libvorbis/vq/vqsplit.c
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-D_LIB
 )
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/include
${LIB_SRC_DIRECTORY}/libogg/include
)

target_link_libraries( ${TARGET} )