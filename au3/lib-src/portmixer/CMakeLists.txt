set(TARGET portmixer)

add_library( ${TARGET} STATIC )

def_vars()

if (UNIX AND NOT APPLE)
   include(CheckIncludeFile)
   set(CMAKE_REQUIRED_LIBRARIES portaudio::portaudio)

   check_include_file( "pa_jack.h" PA_HAS_JACK )

   check_include_file( "pa_linux_alsa.h" PA_HAS_ALSA )
   if( PA_HAS_ALSA )
      find_package( ALSA REQUIRED QUIET )
   endif()

   # There is no easy check, that portaudio was compiled with OSS support
   # Let's pretend that it is always is.
   # Otherwise, OSS code path will never be triggered
   set( PA_HAS_OSS True)
   if( PA_HAS_OSS )
      find_package( OSS REQUIRED QUIET )
   endif()
elseif( WIN32 )
   try_compile(
      PAWINMME_GETSTREAMINPUTHANDLE_EXISTS
      ${CMAKE_CURRENT_BINARY_DIR}/winmme_check
      ${CMAKE_CURRENT_SOURCE_DIR}/cmake/CheckPaWinMME_GetStreamInputHandle
      CheckPaWinMME_GetStreamInputHandle
   )

   try_compile(
      PAWINDS_GETDEVICEGUID_EXISTS
      ${CMAKE_CURRENT_BINARY_DIR}/winds_getguid_check
      ${CMAKE_CURRENT_SOURCE_DIR}/cmake/CheckPaWinDS_GetDeviceGUID
      CheckPaWinDS_GetDeviceGUID
   )
endif()

set( SOURCES
   PRIVATE
      include/portmixer.h

      src/px_mixer.c
      src/px_mixer.h

      $<$<PLATFORM_ID:Windows>:
         src/px_win_common.c
         src/px_win_common.h

         src/px_win_ds.c

         src/px_win_wasapi.c
         src/px_win_endpoint.c

         src/px_win_endpoint.h
         src/px_win_wmme.c
      >

      $<$<BOOL:${APPLE}>:
         src/px_mac_coreaudio.c
      >

      $<$<BOOL:${PA_HAS_OSS}>:
         src/px_unix_oss.c
      >

      $<$<BOOL:${PA_HAS_ALSA}>:
         src/px_linux_alsa.c
      >
)

set( INCLUDES
   PRIVATE
      src
   PUBLIC
      include
)

set( DEFINES
   PUBLIC
      USE_PORTMIXER=1
   PRIVATE
      $<$<PLATFORM_ID:Windows>:
         PX_USE_WIN_DSOUND=1
         PX_USE_WIN_WASAPI=1
         PX_USE_WIN_MME=1
      >

      $<$<BOOL:${APPLE}>:
         PX_USE_MAC_COREAUDIO=1
      >

      $<$<BOOL:${PA_HAS_OSS}>:
         PX_USE_UNIX_OSS=1
      >

      $<$<BOOL:${PA_HAS_ALSA}>:
         PX_USE_LINUX_ALSA=1
      >

      $<$<BOOL:${PAWINMME_GETSTREAMINPUTHANDLE_EXISTS}>:
         PX_PAWINMME_GETSTREAMINPUTHANDLE_EXISTS=1
      >

      $<$<BOOL:${PAWINDS_GETDEVICEGUID_EXISTS}>:
         PX_PAWINDS_GETDEVICEGUID_EXISTS=1
      >
)

set( LIBRARIES
   PRIVATE
      portaudio::portaudio
)

if( PA_HAS_ALSA )
   list( APPEND LIBRARIES ALSA::ALSA )
endif()

if( PA_HAS_OSS )
   list( APPEND LIBRARIES OSS::OSS )
endif()

set_target_properties( ${TARGET} PROPERTIES FOLDER "lib-src" )

organize_source( "${CMAKE_CURRENT_SOURCE_DIR}" "" "${SOURCES}" )

target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )
set_target_properties(${TARGET} PROPERTIES POSITION_INDEPENDENT_CODE On)
