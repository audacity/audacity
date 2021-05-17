
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/ALAC/ALACBitUtilities.c
      ${TARGET_ROOT}/src/ALAC/ag_dec.c
      ${TARGET_ROOT}/src/ALAC/ag_enc.c
      ${TARGET_ROOT}/src/ALAC/alac_decoder.c
      ${TARGET_ROOT}/src/ALAC/alac_encoder.c
      ${TARGET_ROOT}/src/ALAC/dp_dec.c
      ${TARGET_ROOT}/src/ALAC/dp_enc.c
      ${TARGET_ROOT}/src/ALAC/matrix_dec.c
      ${TARGET_ROOT}/src/ALAC/matrix_enc.c
      ${TARGET_ROOT}/src/G72x/g721.c
      ${TARGET_ROOT}/src/G72x/g723_16.c
      ${TARGET_ROOT}/src/G72x/g723_24.c
      ${TARGET_ROOT}/src/G72x/g723_40.c
      ${TARGET_ROOT}/src/G72x/g72x.c
      ${TARGET_ROOT}/src/GSM610/add.c
      ${TARGET_ROOT}/src/GSM610/code.c
      ${TARGET_ROOT}/src/GSM610/decode.c
      ${TARGET_ROOT}/src/GSM610/gsm_create.c
      ${TARGET_ROOT}/src/GSM610/gsm_decode.c
      ${TARGET_ROOT}/src/GSM610/gsm_destroy.c
      ${TARGET_ROOT}/src/GSM610/gsm_encode.c
      ${TARGET_ROOT}/src/GSM610/gsm_option.c
      ${TARGET_ROOT}/src/GSM610/long_term.c
      ${TARGET_ROOT}/src/GSM610/lpc.c
      ${TARGET_ROOT}/src/GSM610/preprocess.c
      ${TARGET_ROOT}/src/GSM610/rpe.c
      ${TARGET_ROOT}/src/GSM610/short_term.c
      ${TARGET_ROOT}/src/GSM610/table.c
      ${TARGET_ROOT}/src/aiff.c
      ${TARGET_ROOT}/src/alac.c
      ${TARGET_ROOT}/src/alaw.c
      ${TARGET_ROOT}/src/au.c
      ${TARGET_ROOT}/src/audio_detect.c
      ${TARGET_ROOT}/src/avr.c
      ${TARGET_ROOT}/src/broadcast.c
      ${TARGET_ROOT}/src/caf.c
      ${TARGET_ROOT}/src/cart.c
      ${TARGET_ROOT}/src/chanmap.c
      ${TARGET_ROOT}/src/chunk.c
      ${TARGET_ROOT}/src/command.c
      ${TARGET_ROOT}/src/common.c
      ${TARGET_ROOT}/src/dither.c
      ${TARGET_ROOT}/src/double64.c
      ${TARGET_ROOT}/src/dwd.c
      ${TARGET_ROOT}/src/dwvw.c
      ${TARGET_ROOT}/src/file_io.c
      ${TARGET_ROOT}/src/flac.c
      ${TARGET_ROOT}/src/float32.c
      ${TARGET_ROOT}/src/g72x.c
      ${TARGET_ROOT}/src/gsm610.c
      ${TARGET_ROOT}/src/htk.c
      ${TARGET_ROOT}/src/id3.c
      ${TARGET_ROOT}/src/ima_adpcm.c
      ${TARGET_ROOT}/src/ima_oki_adpcm.c
      ${TARGET_ROOT}/src/interleave.c
      ${TARGET_ROOT}/src/ircam.c
      ${TARGET_ROOT}/src/macos.c
      ${TARGET_ROOT}/src/mat4.c
      ${TARGET_ROOT}/src/mat5.c
      ${TARGET_ROOT}/src/mpc2k.c
      ${TARGET_ROOT}/src/ms_adpcm.c
      ${TARGET_ROOT}/src/nist.c
      ${TARGET_ROOT}/src/nms_adpcm.c
      ${TARGET_ROOT}/src/ogg.c
      ${TARGET_ROOT}/src/ogg_opus.c
      ${TARGET_ROOT}/src/ogg_pcm.c
      ${TARGET_ROOT}/src/ogg_speex.c
      ${TARGET_ROOT}/src/ogg_vcomment.c
      ${TARGET_ROOT}/src/ogg_vorbis.c
      ${TARGET_ROOT}/src/paf.c
      ${TARGET_ROOT}/src/pcm.c
      ${TARGET_ROOT}/src/pvf.c
      ${TARGET_ROOT}/src/raw.c
      ${TARGET_ROOT}/src/rf64.c
      ${TARGET_ROOT}/src/rx2.c
      ${TARGET_ROOT}/src/sd2.c
      ${TARGET_ROOT}/src/sds.c
      ${TARGET_ROOT}/src/sndfile.c
      ${TARGET_ROOT}/src/strings.c
      ${TARGET_ROOT}/src/svx.c
      ${TARGET_ROOT}/src/txw.c
      ${TARGET_ROOT}/src/ulaw.c
      ${TARGET_ROOT}/src/voc.c
      ${TARGET_ROOT}/src/vox_adpcm.c
      ${TARGET_ROOT}/src/w64.c
      ${TARGET_ROOT}/src/wav.c
      ${TARGET_ROOT}/src/wavlike.c
      ${TARGET_ROOT}/src/wve.c
      ${TARGET_ROOT}/src/xi.c
)

list( APPEND INCLUDES
   PRIVATE
      ${_PRVDIR}
      ${TARGET_ROOT}/src
   PUBLIC
      ${_PUBDIR}
)

list( APPEND DEFINES
   PRIVATE
      inline=__inline
      LIBSNDFILE_PRIVATE_CONFIG
)

list( APPEND OPTIONS
   PRIVATE
      $<$<C_COMPILER_ID:MSVC>:/wd4996>
      $<$<C_COMPILER_ID:AppleClang>:-Wno-implicit-function-declaration>
)

if( CMAKE_C_COMPILER_ID MATCHES "MSVC" )
   cmake_push_check_state( RESET )
   set( CMAKE_EXTRA_INCLUDE_FILES "BaseTsd.h" )
   check_type_size( "SSIZE_T" SIZEOF_SSIZE_T LANGUAGE C )
   cmake_pop_check_state()

   if( HAVE_SIZEOF_SSIZE_T )
      list( APPEND DEFINES ssize_t=SSIZE_T )
   endif()
else()
   check_type_size( "ssize_t" SIZEOF_SSIZE_T LANGUAGE C )
endif()

if( HAVE_SIZEOF_SSIZE_T )
   set( HAVE_SSIZE_T YES )
endif()

set( TYPEOF_SF_COUNT_T "int64_t" )
check_type_size( "${TYPEOF_SF_COUNT_T}" SIZEOF_SF_COUNT_T LANGUAGE C )
set( SF_COUNT_MAX "0x7FFFFFFFFFFFFFFFLL" )

configure_file( ${TARGET_ROOT}/src/sndfile.h.in ${_PUBDIR}/sndfile.h )

check_symbol_exists( S_IRGRP "sys/stat.h" HAVE_DECL_S_IRGRP )

set( COMPILER_IS_GCC OFF )
if( CMAKE_C_COMPILER_ID MATCHES "GNU|.*Clang" )
   set( COMPILER_IS_GCC ON )
endif()

set( OS_IS_MACOSX OFF)
if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
   set( OS_IS_MACOSX ON )
endif()

set( OS_IS_WIN32 OFF )
set( USE_WINDOWS_API OFF )
if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   set( OS_IS_MACOSX ON )
   set( USE_WINDOWS_API ON )
endif()

set( CPU_IS_BIG_ENDIAN ${WORDS_BIGENDIAN} )
set( CPU_IS_LITTLE_ENDIAN !${WORDS_BIGENDIAN} )
set( HAVE_EXTERNAL_LIBS 0 )

set( VERSION "1.0.24" )
set( PACKAGE "libsndfile" )
set( PACKAGE_NAME ${PACKAGE} )
set( PACKAGE_VERSION ${VERSION} )

# Does compiler support "flexible array members"
try_compile( HAVE_FLEXIBLE_ARRAY
   ${CMAKE_CURRENT_BINARY_DIR}
   ${_SRCDIR}/famcheck.c
   LINK_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES}
   OUTPUT_VARIABLE c_out
)

# Determine how the CPU clips when doing float to int conversions
try_run( r_rc c_rc
   ${CMAKE_CURRENT_BINARY_DIR}
   ${_SRCDIR}/clipcheck.c
   LINK_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES}
   RUN_OUTPUT_VARIABLE r_out
   COMPILE_OUTPUT_VARIABLE c_out
)

if( NOT c_rc )
   message( STATUS "${c_out}" )
   message( FATAL_ERROR "${_SRCDIR}/clipcheck.c compile failed:" )
endif()

list( GET r_out 0 CPU_CLIPS_POSITIVE )
list( GET r_out 1 CPU_CLIPS_NEGATIVE )

configure_file( ${TARGET_ROOT}/src/config.h.cmake ${_PRVDIR}/config.h )

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )

