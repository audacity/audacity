#directory cmake-proxies/libflac
set( TARGET libflac )
set( TARGET_SOURCE ${LIB_SRC_DIRECTORY}${TARGET} )
project( ${TARGET} )

set( SOURCES 
#${LIB_SRC_DIRECTORY}libflac/src/flac/analyze.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/decode.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/encode.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/foreign_metadata.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/iffscan.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/local_string_utils.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/main.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/utils.c
#${LIB_SRC_DIRECTORY}libflac/src/flac/vorbiscomment.c

${LIB_SRC_DIRECTORY}libflac/src/libFLAC/bitmath.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/bitreader.c


${LIB_SRC_DIRECTORY}libflac/src/libFLAC/bitwriter.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/cpu.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/crc.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/fixed.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/fixed_intrin_sse2.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/fixed_intrin_ssse3.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/float.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/format.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/lpc.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/lpc_intrin_avx2.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/lpc_intrin_sse.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/lpc_intrin_sse2.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/lpc_intrin_sse41.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/md5.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/memory.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/metadata_iterators.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/metadata_object.c
#${LIB_SRC_DIRECTORY}libflac/src/libFLAC/ogg_decoder_aspect.c
#${LIB_SRC_DIRECTORY}libflac/src/libFLAC/ogg_encoder_aspect.c
#${LIB_SRC_DIRECTORY}libflac/src/libFLAC/ogg_helper.c
#${LIB_SRC_DIRECTORY}libflac/src/libFLAC/ogg_mapping.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/stream_decoder.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/stream_encoder.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/stream_encoder_framing.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/stream_encoder_intrin_avx2.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/stream_encoder_intrin_sse2.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/stream_encoder_intrin_ssse3.c
${LIB_SRC_DIRECTORY}libflac/src/libFLAC/window.c

#${LIB_SRC_DIRECTORY}libflac/src/metaflac/main.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/operations.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/operations_shorthand_cuesheet.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/operations_shorthand_picture.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/operations_shorthand_seektable.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/operations_shorthand_streaminfo.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/operations_shorthand_vorbiscomment.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/options.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/usage.c
#${LIB_SRC_DIRECTORY}libflac/src/metaflac/utils.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_common/charset.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_common/dither.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_common/replaygain.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_common/tags.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_xmms/charset.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_xmms/configure.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_xmms/fileinfo.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_xmms/http.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_xmms/plugin.c
#${LIB_SRC_DIRECTORY}libflac/src/plugin_xmms/tag.c
#${LIB_SRC_DIRECTORY}libflac/src/share/getopt/getopt.c
#${LIB_SRC_DIRECTORY}libflac/src/share/getopt/getopt1.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/alloc.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/cuesheet.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/file.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/picture.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/replaygain.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/seektable.c
#${LIB_SRC_DIRECTORY}libflac/src/share/grabbag/snprintf.c
#${LIB_SRC_DIRECTORY}libflac/src/share/replaygain_analysis/replaygain_analysis.c
#${LIB_SRC_DIRECTORY}libflac/src/share/replaygain_synthesis/replaygain_synthesis.c
#${LIB_SRC_DIRECTORY}libflac/src/share/utf8/charset.c
#${LIB_SRC_DIRECTORY}libflac/src/share/utf8/charset_test.c
#${LIB_SRC_DIRECTORY}libflac/src/share/utf8/iconvert.c
#${LIB_SRC_DIRECTORY}libflac/src/share/utf8/makemap.c
#${LIB_SRC_DIRECTORY}libflac/src/share/utf8/utf8.c
${LIB_SRC_DIRECTORY}libflac/src/share/win_utf8_io/win_utf8_io.c
#${LIB_SRC_DIRECTORY}libflac/src/test_grabbag/cuesheet/main.c
#${LIB_SRC_DIRECTORY}libflac/src/test_grabbag/picture/main.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/bitwriter.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/decoders.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/encoders.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/endswap.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/format.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/main.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/md5.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/metadata.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/metadata_manip.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libFLAC/metadata_object.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libs_common/file_utils_flac.c
#${LIB_SRC_DIRECTORY}libflac/src/test_libs_common/metadata_utils.c
#${LIB_SRC_DIRECTORY}libflac/src/test_seeking/main.c
#${LIB_SRC_DIRECTORY}libflac/src/test_streams/main.c


#${LIB_SRC_DIRECTORY}libflac/src/utils/flacdiff/main.cpp
#${LIB_SRC_DIRECTORY}libflac/src/utils/flactimer/main.cpp
)
# This defines the #define on both Windows and Linux.
add_definitions( 
-DWIN32 
-D_LIB
-DFLAC__CPU_IA32
-DFLAC__SSE_OS
-DFLAC__HAS_X86INTRIN
-DFLAC__ALIGN_MALLOC_DATA
-DVERSION="1.3.1"
-DFLAC__NO_DLL
-DFLAC__OVERFLOW_DETECT
)
add_library( ${TARGET} STATIC ${SOURCES})

target_include_directories( ${TARGET} PRIVATE 
${TARGET_SOURCE}/src/libFLAC/include
${TARGET_SOURCE}/include
)

target_link_libraries( ${TARGET} )