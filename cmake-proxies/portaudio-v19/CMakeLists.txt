
add_library( ${TARGET} STATIC )

list( APPEND SOURCES
   PRIVATE
      ${TARGET_ROOT}/src/common/pa_allocation.c
      ${TARGET_ROOT}/src/common/pa_converters.c
      ${TARGET_ROOT}/src/common/pa_cpuload.c
      ${TARGET_ROOT}/src/common/pa_debugprint.c
      ${TARGET_ROOT}/src/common/pa_dither.c
      ${TARGET_ROOT}/src/common/pa_dynload.c
      ${TARGET_ROOT}/src/common/pa_front.c
      ${TARGET_ROOT}/src/common/pa_process.c
      ${TARGET_ROOT}/src/common/pa_ringbuffer.c
      ${TARGET_ROOT}/src/common/pa_stream.c
      ${TARGET_ROOT}/src/common/pa_trace.c
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/src/common
   PUBLIC
      ${TARGET_ROOT}/include
)

if( WIN32 )
   list( APPEND DEFINES
      PUBLIC
         PA_USE_DS=1
         PA_USE_WASAPI=1
         PA_USE_WMME=1
   )

   list( APPEND SOURCES
      PRIVATE
         ${TARGET_ROOT}/src/hostapi/dsound/pa_win_ds.c
         ${TARGET_ROOT}/src/hostapi/dsound/pa_win_ds_dynlink.c
         ${TARGET_ROOT}/src/hostapi/wasapi/pa_win_wasapi.c
         ${TARGET_ROOT}/src/hostapi/wmme/pa_win_wmme.c
         ${TARGET_ROOT}/src/os/win/pa_win_coinitialize.c
         ${TARGET_ROOT}/src/os/win/pa_win_hostapis.c
         ${TARGET_ROOT}/src/os/win/pa_win_util.c
         ${TARGET_ROOT}/src/os/win/pa_win_waveformat.c
         ${TARGET_ROOT}/src/os/win/pa_win_wdmks_utils.c
         ${TARGET_ROOT}/src/os/win/pa_x86_plain_converters.c
   )

   list( APPEND INCLUDES
      PRIVATE
         ${TARGET_ROOT}/src/hostapi/dsound
         ${TARGET_ROOT}/src/os/win
   )

elseif( APPLE )
   list( APPEND DEFINES
      PUBLIC
         PA_USE_COREAUDIO=1
   )

   list( APPEND SOURCES
      PRIVATE
         ${TARGET_ROOT}/src/hostapi/coreaudio/pa_mac_core.c
         ${TARGET_ROOT}/src/hostapi/coreaudio/pa_mac_core_blocking.c
         ${TARGET_ROOT}/src/hostapi/coreaudio/pa_mac_core_utilities.c
         ${TARGET_ROOT}/src/os/unix/pa_unix_hostapis.c
         ${TARGET_ROOT}/src/os/unix/pa_unix_util.c
   )

   list( APPEND INCLUDES
      PRIVATE
         ${TARGET_ROOT}/src/hostapi/coreaudio
         ${TARGET_ROOT}/src/os/unix
   )

   list( APPEND LIBRARIES
      INTERFACE
         "-framework CoreAudio"
   )
elseif( UNIX )
   find_package(ALSA)
   if( ALSA_FOUND )
      list( APPEND DEFINES
         PUBLIC
            PA_USE_ALSA=1
      )

      list( APPEND SOURCES
         PRIVATE
            ${TARGET_ROOT}/src/hostapi/alsa/pa_linux_alsa.c
      )

      list( APPEND INCLUDES
         PRIVATE
            ${ALSA_INCLUDE_DIRS}
      )

      list( APPEND LIBRARIES
         INTERFACE
            ${ALSA_LIBRARIES}
      )
   endif()

   list( APPEND SOURCES
      PRIVATE
         ${TARGET_ROOT}/src/os/unix/pa_unix_hostapis.c
         ${TARGET_ROOT}/src/os/unix/pa_unix_util.c
   )

   list( APPEND INCLUDES
      PRIVATE
         ${TARGET_ROOT}/src/os/unix
   )
endif()

set( CMAKE_MODULE_PATH ${TARGET_ROOT}/cmake_support )

find_package(Jack)
if( JACK_FOUND )
   list( APPEND DEFINES
      PUBLIC
        PA_USE_JACK=1
   )

   list( APPEND SOURCES
      PRIVATE
         ${TARGET_ROOT}/src/hostapi/jack/pa_jack.c
         ${TARGET_ROOT}/src/hostapi/jack/pa_jack_dynload.c
   )

   list( APPEND INCLUDES
      PRIVATE
         ${TARGET_ROOT}/src/hostapi/jack
         ${JACK_INCLUDE_DIRS}      
   )

   list( APPEND LIBRARIES
      INTERFACE
         ${JACK_LIBRARIES}
   )
endif()

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

