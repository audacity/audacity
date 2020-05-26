
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE

      # libflac
      ${TARGET_ROOT}/src/libFLAC/bitmath.c
      ${TARGET_ROOT}/src/libFLAC/bitreader.c
      ${TARGET_ROOT}/src/libFLAC/bitwriter.c
      ${TARGET_ROOT}/src/libFLAC/cpu.c
      ${TARGET_ROOT}/src/libFLAC/crc.c
      ${TARGET_ROOT}/src/libFLAC/fixed.c
      ${TARGET_ROOT}/src/libFLAC/fixed_intrin_sse2.c
      ${TARGET_ROOT}/src/libFLAC/fixed_intrin_ssse3.c
      ${TARGET_ROOT}/src/libFLAC/float.c
      ${TARGET_ROOT}/src/libFLAC/format.c
      ${TARGET_ROOT}/src/libFLAC/lpc.c
      ${TARGET_ROOT}/src/libFLAC/lpc_intrin_sse.c
      ${TARGET_ROOT}/src/libFLAC/lpc_intrin_sse2.c
      ${TARGET_ROOT}/src/libFLAC/lpc_intrin_sse41.c
      ${TARGET_ROOT}/src/libFLAC/lpc_intrin_avx2.c
      ${TARGET_ROOT}/src/libFLAC/md5.c
      ${TARGET_ROOT}/src/libFLAC/memory.c
      ${TARGET_ROOT}/src/libFLAC/metadata_iterators.c
      ${TARGET_ROOT}/src/libFLAC/metadata_object.c
      ${TARGET_ROOT}/src/libFLAC/stream_decoder.c
      ${TARGET_ROOT}/src/libFLAC/stream_encoder.c
      ${TARGET_ROOT}/src/libFLAC/stream_encoder_intrin_sse2.c
      ${TARGET_ROOT}/src/libFLAC/stream_encoder_intrin_ssse3.c
      ${TARGET_ROOT}/src/libFLAC/stream_encoder_intrin_avx2.c
      ${TARGET_ROOT}/src/libFLAC/stream_encoder_framing.c
      ${TARGET_ROOT}/src/libFLAC/window.c
      ${TARGET_ROOT}/src/libFLAC/ogg_decoder_aspect.c
      ${TARGET_ROOT}/src/libFLAC/ogg_encoder_aspect.c
      ${TARGET_ROOT}/src/libFLAC/ogg_helper.c
      ${TARGET_ROOT}/src/libFLAC/ogg_mapping.c
      $<$<BOOL:${WIN32}>:${TARGET_ROOT}/src/share/win_utf8_io/win_utf8_io.c>

      # libflac++
      ${TARGET_ROOT}/src/libFLAC++/metadata.cpp
      ${TARGET_ROOT}/src/libFLAC++/stream_decoder.cpp
      ${TARGET_ROOT}/src/libFLAC++/stream_encoder.cpp
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
      ${TARGET_ROOT}/src/libFLAC/include
   PUBLIC
      ${TARGET_ROOT}/include
)

list( APPEND DEFINES
   PRIVATE
      HAVE_CONFIG_H
   PUBLIC
      FLAC__NO_DLL
)

list( APPEND LIBRARIES
   PUBLIC
      libogg
)

set( CPU_IS_BIG_ENDIAN ${WORDS_BIGENDIAN} )
set( CPU_IS_LITTLE_ENDIAN !${WORDS_BIGENDIAN} )

set( FLAC__HAS_OGG 1 )

set( VERSION "1.3.1" )

configure_file( config.h.in private/config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

