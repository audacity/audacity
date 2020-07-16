
add_library( ${TARGET} STATIC )

def_vars()

set( CMAKE_MODULE_PATH ${TARGET_ROOT}/cmake_support )

# Define the platform specific interface options
if( CMAKE_SYSTEM_NAME MATCHES "Windows" )
   if(DEFINED ENV{ASIOSDK_DIR} )
      cmd_option(
         ${_OPT}use_pa_asio
         "Use the portaudio ASIO interface if available"
         YES
      )
   endif()
   cmd_option(
      ${_OPT}use_pa_ds
      "Use the portaudio DirectSound interface if available"
      YES
   )
   cmd_option(
      ${_OPT}use_pa_wasapi
      "Use the portaudio WASAPI interface if available"
      YES
   )
   cmd_option(
      ${_OPT}use_pa_wmme
      "Use the portaudio WMME interface if available"
      YES
   )
else()
   # Look for OSS if the user wants it
   cmd_option(
      ${_OPT}use_pa_oss
      "Use the OSS audio interface if available"
      YES
   )

   if( ${_OPT}use_pa_oss )
      find_path( OSS_INCLUDE NAMES sys/soundcard.h )
      mark_as_advanced( FORCE OSS_INCLUDE )

      if( OSS_INCLUDE )
         set( OSS_INCLUDE_DIRS ${OSS_INCLUDE} )
      endif()

      find_library( OSS_LIBRARY NAMES ossaudio )
      mark_as_advanced( FORCE OSS_LIBRARY )

      if( OSS_LIBRARY )
         set( OSS_LIBRARIES ${OSS_LIBRARY} )
      endif()

      if( NOT OSS_INCLUDE_DIRS )
         set_cache_value( ${_OPT}use_pa_oss NO )
      endif()
   endif()

   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      cmd_option(
         ${_OPT}use_pa_coreaudio
         "Use the portaudio CoreAudio interface if available"
         YES
      )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Linux|FreeBSD" )
      cmd_option(
         ${_OPT}use_pa_alsa
         "Use the portaudio ALSA interface if available"
         YES
      )

      if( ${_OPT}use_pa_alsa )
         find_package( ALSA )
         if( NOT ALSA_FOUND )
            set_cache_value( ${_OPT}use_pa_alsa NO )
         endif()
      endif()
   endif()
endif()

# JACK can be dynamically loaded, linked to, or off
cmd_option( ${_OPT}use_pa_jack
            "Use the JACK audio interface if available [loaded, linked, off]"
            "linked"
            STRINGS "loaded" "linked" "off"
)

if( NOT ${_OPT}use_pa_jack STREQUAL "off" )
   # Find it
   find_package( Jack )
   if( NOT JACK_FOUND)
      set_cache_value( ${_OPT}use_pa_jack "off" )
   endif()
endif()

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

      $<$<PLATFORM_ID:Windows>:
         ${TARGET_ROOT}/src/os/win/pa_win_coinitialize.c
         ${TARGET_ROOT}/src/os/win/pa_win_hostapis.c
         ${TARGET_ROOT}/src/os/win/pa_win_util.c
         ${TARGET_ROOT}/src/os/win/pa_win_waveformat.c
         ${TARGET_ROOT}/src/os/win/pa_win_wdmks_utils.c
         $<$<C_COMPILER_ID:MSVC>:
            ${TARGET_ROOT}/src/os/win/pa_x86_plain_converters.c
         >
      >

      $<$<PLATFORM_ID:Darwin>:
         ${TARGET_ROOT}/src/hostapi/coreaudio/pa_mac_core.c
         ${TARGET_ROOT}/src/hostapi/coreaudio/pa_mac_core_blocking.c
         ${TARGET_ROOT}/src/hostapi/coreaudio/pa_mac_core_utilities.c
      >

      $<$<PLATFORM_ID:Darwin,Linux,FreeBSD,CYGWIN>:
         ${TARGET_ROOT}/src/os/unix/pa_unix_hostapis.c
         ${TARGET_ROOT}/src/os/unix/pa_unix_util.c
      >

      $<$<BOOL:${${_OPT}use_pa_asio}>:
         ${TARGET_ROOT}/src/hostapi/asio/iasiothiscallresolver.cpp
         ${TARGET_ROOT}/src/hostapi/asio/pa_asio.cpp
      >

      $<$<BOOL:${${_OPT}use_pa_ds}>:
         ${TARGET_ROOT}/src/hostapi/dsound/pa_win_ds.c
         ${TARGET_ROOT}/src/hostapi/dsound/pa_win_ds_dynlink.c
      >

      $<$<BOOL:${${_OPT}use_pa_wasapi}>:
         ${TARGET_ROOT}/src/hostapi/wasapi/pa_win_wasapi.c
      >

      $<$<BOOL:${${_OPT}use_pa_wmme}>:
         ${TARGET_ROOT}/src/hostapi/wmme/pa_win_wmme.c
      >

      $<$<BOOL:${${_OPT}use_pa_alsa}>:
         ${TARGET_ROOT}/src/hostapi/alsa/pa_linux_alsa.c
      >

      $<$<BOOL:${${_OPT}use_pa_oss}>:
         ${TARGET_ROOT}/src/hostapi/oss/pa_unix_oss.c
      >

      $<$<NOT:$<STREQUAL:${${_OPT}use_pa_jack},off>>:
         ${TARGET_ROOT}/src/hostapi/jack/pa_jack.c
         ${TARGET_ROOT}/src/hostapi/jack/pa_jack_dynload.c
      >
)

list( APPEND EXTRASRC
   PRIVATE
      $<$<BOOL:${${_OPT}use_pa_asio}>:
         $ENV{ASIOSDK_DIR}/common/asio.cpp
         $ENV{ASIOSDK_DIR}/host/asiodrivers.cpp
         $ENV{ASIOSDK_DIR}/host/pc/asiolist.cpp
      >
)

list( APPEND INCLUDES
   PRIVATE
      ${TARGET_ROOT}/src/common

      $<$<PLATFORM_ID:Windows>:
         ${TARGET_ROOT}/src/os/win
      >

      $<$<PLATFORM_ID:Darwin,Linux,FreeBSD,CYGWIN>:
         ${TARGET_ROOT}/src/os/unix
      >

      $<$<BOOL:${${_OPT}use_pa_asio}>:
         ${TARGET_ROOT}/src/hostapi/asio
         $ENV{ASIOSDK_DIR}/common
         $ENV{ASIOSDK_DIR}/host
         $ENV{ASIOSDK_DIR}/host/pc
      >

      $<$<BOOL:${${_OPT}use_pa_ds}>:
         ${TARGET_ROOT}/src/hostapi/dsound
      >

      $<$<BOOL:${${_OPT}use_pa_coreaudio}>:
         ${TARGET_ROOT}/src/hostapi/coreaudio
      >

      $<$<BOOL:${${_OPT}use_pa_alsa}>:
         ${ALSA_INCLUDE_DIRS}
      >

      $<$<BOOL:${${_OPT}use_pa_oss}>:
         ${OSS_INCLUDE_DIRS}
      >

      $<$<NOT:$<STREQUAL:${${_OPT}use_pa_jack},off>>:
         ${TARGET_ROOT}/src/hostapi/jack
         ${JACK_INCLUDE_DIRS}
      >

   PUBLIC
      ${TARGET_ROOT}/include
)

list( APPEND DEFINES
   PUBLIC
      $<$<BOOL:${${_OPT}use_pa_asio}>:
         PA_USE_ASIO=1
      >

      $<$<BOOL:${${_OPT}use_pa_ds}>:
         PA_USE_DS=1
      >

      $<$<BOOL:${${_OPT}use_pa_wasapi}>:
         PA_USE_WASAPI=1
      >

      $<$<BOOL:${${_OPT}use_pa_wmme}>:
         PA_USE_WMME=1
      >

      $<$<BOOL:${${_OPT}use_pa_coreaudio}>:
         PA_USE_COREAUDIO=1
      >

      $<$<BOOL:${${_OPT}use_pa_alsa}>:
         PA_USE_ALSA=1
      >

      $<$<BOOL:${${_OPT}use_pa_oss}>:
         PA_USE_OSS=1
         HAVE_SYS_SOUNDCARD_H=1
      >

      $<$<NOT:$<STREQUAL:${${_OPT}use_pa_jack},off>>:
         PA_USE_JACK=1
      >

      $<$<STREQUAL:${${_OPT}use_pa_jack},dynamic>:
         PA_DYNAMIC_JACK=1
      >
)

list( APPEND LIBRARIES
   INTERFACE
      $<$<BOOL:${${_OPT}use_pa_alsa}>:
         ${ALSA_LIBRARIES}
      >

      $<$<BOOL:${${_OPT}use_pa_oss}>:
         ${OSS_LIBRARIES}
      >

      $<$<NOT:$<STREQUAL:${${_OPT}use_pa_jack},off>>:
         ${JACK_LIBRARIES}
      >
)

organize_source( "${TARGET_ROOT}" "" "${SOURCES}" )
target_sources( ${TARGET} PRIVATE ${SOURCES} ${EXTRASRC} )
target_compile_definitions( ${TARGET} PRIVATE ${DEFINES} )
target_include_directories( ${TARGET} PRIVATE ${INCLUDES} )
target_link_libraries( ${TARGET} PRIVATE ${LIBRARIES} )

