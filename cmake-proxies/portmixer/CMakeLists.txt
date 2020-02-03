
add_library( ${TARGET} STATIC )

get_target_property( PA_DEFS portaudio-v19 COMPILE_DEFINITIONS )
string( REPLACE ";" " " PA_DEFS "${PA_DEFS}" )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/px_mixer.c
      $<$<PLATFORM_ID:Windows>:${TARGET_ROOT}/src/px_win_common.c>
      $<$<IN_LIST:PA_USE_DS=1,${PA_DEFS}>:${TARGET_ROOT}/src/px_win_ds.c>
      $<$<IN_LIST:PA_USE_WASAPI=1,${PA_DEFS}>:${TARGET_ROOT}/src/px_win_wasapi.c>
      $<$<IN_LIST:PA_USE_WMME=1,${PA_DEFS}>:${TARGET_ROOT}/src/px_win_wmme.c>
      $<$<IN_LIST:PA_USE_COREAUDIO=1,${PA_DEFS}>:${TARGET_ROOT}/src/px_mac_coreaudio.c>
      $<$<IN_LIST:PA_USE_ALSA=1,${PA_DEFS}>:${TARGET_ROOT}/src/px_linux_alsa.c>
      $<$<IN_LIST:PA_USE_OSS=1,${PA_DEFS}>:${TARGET_ROOT}/src/px_unix_oss.c>
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/src
   PUBLIC
      ${TARGET_ROOT}/include
)

list( APPEND DEFINES
   PRIVATE
      $<$<IN_LIST:PA_USE_DS=1,${PA_DEFS}>:PX_USE_WIN_DSOUND=1>
      $<$<IN_LIST:PA_USE_WASAPI=1,${PA_DEFS}>:PX_USE_WIN_WASAPI=1>
      $<$<IN_LIST:PA_USE_WMME=1,${PA_DEFS}>:PX_USE_WIN_MME=1>
      $<$<IN_LIST:PA_USE_COREAUDIO=1,${PA_DEFS}>:PX_USE_MAC_COREAUDIO=1>
      $<$<IN_LIST:PA_USE_ALSA=1,${PA_DEFS}>:PX_USE_LINUX_ALSA=1>
      $<$<IN_LIST:PA_USE_OSS=1,${PA_DEFS}>:PX_USE_UNIX_OSS=1>
)

list( APPEND LIBRARIES
   PRIVATE
      portaudio-v19
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_compile_features( ${TARGET} PRIVATE ${FEATURES} )
target_compile_options( ${TARGET} PRIVATE ${OPTIONS} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

