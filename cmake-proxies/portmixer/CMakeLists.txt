
add_library( ${TARGET} STATIC )

def_vars()

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/px_mixer.c
      $<$<PLATFORM_ID:Windows>:
         ${TARGET_ROOT}/src/px_win_common.c
      >

      $<$<BOOL:${use_pa_ds}>:
         ${TARGET_ROOT}/src/px_win_ds.c
      >

      $<$<BOOL:${use_pa_wasapi}>:
         ${TARGET_ROOT}/src/px_win_wasapi.c
         ${TARGET_ROOT}/src/px_win_endpoint.c
      >

      $<$<BOOL:${use_pa_wmme}>:
         ${TARGET_ROOT}/src/px_win_wmme.c
      >

      $<$<BOOL:${use_pa_coreaudio}>:
         ${TARGET_ROOT}/src/px_mac_coreaudio.c
      >

      $<$<BOOL:${use_pa_alsa}>:
         ${TARGET_ROOT}/src/px_linux_alsa.c
      >

      $<$<BOOL:${use_pa_oss}>:
         ${TARGET_ROOT}/src/px_unix_oss.c
      >
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/src
   PUBLIC
      ${TARGET_ROOT}/include
)

list( APPEND DEFINES
   PRIVATE
      $<$<BOOL:${use_pa_ds}>:
         PX_USE_WIN_DSOUND=1
      >

      $<$<BOOL:${use_pa_wasapi}>:
         PX_USE_WIN_WASAPI=1
      >

      $<$<BOOL:${use_pa_wmme}>:
         PX_USE_WIN_MME=1
      >

      $<$<BOOL:${use_pa_coreaudio}>:
         PX_USE_MAC_COREAUDIO=1
      >

      $<$<BOOL:${use_pa_alsa}>:
         PX_USE_LINUX_ALSA=1
      >

      $<$<BOOL:${use_pa_oss}>:
         PX_USE_UNIX_OSS=1
      >
)

list( APPEND LIBRARIES
   PRIVATE
      portaudio-v19
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

