add_conan_lib(
   ZLIB
   zlib/1.2.11
   REQUIRED
   PKG_CONFIG "zlib >= 1.2.11"
   INTERFACE_NAME ZLIB::ZLIB
   OPTION_NAME zlib
   CONAN_OPTIONS
      zlib:shared=True
)

add_conan_lib(
   expat
   expat/2.2.9@audacity/stable
   REQUIRED
   PKG_CONFIG "expat >= 2.1.0"
   CONAN_OPTIONS
      expat:shared=True
)

if(${_OPT}has_crashreports)
   add_conan_lib(breakpad breakpad/0.1 REQUIRED)
endif()

set( wx_zlib "zlib" )

set( wx_png "libpng" )
set( wx_jpeg "libjpeg-turbo")
set( wx_tiff "off" )

set( id3tag_zlib "conan" )

if ( ${_OPT}use_zlib STREQUAL "system" )
   set( wx_zlib "sys" )
   # To prevent linking conflicts - we need to use system libpng as well.
   # wxWdigets will attempt to resolve it using find_package
   find_required_package( PNG "libpng-dev" )
   set( wx_png "sys" )
   # And, for consistency
   find_required_package( JPEG "libjpeg-turbo8-dev" )
   set( wx_jpeg "sys" )

   set( id3tag_zlib "system" )
endif()

set(wx_expat "expat")

if (${_OPT}use_expat STREQUAL "system")
   set(wx_expat "sys")
endif()

add_conan_lib(
   wxWidgets
   wxwidgets/3.1.3.1-audacity
   REQUIRED
   ALWAYS_ALLOW_CONAN_FALLBACK
   OPTION_NAME wxwidgets
   FIND_PACKAGE_OPTIONS COMPONENTS adv base core html qa xml net
   INTERFACE_NAME wxwidgets::wxwidgets
   CONAN_OPTIONS
      wxwidgets:shared=True
      wxwidgets:zlib=${wx_zlib}
      wxwidgets:expat=${wx_expat}
      wxwidgets:compatibility=3.0
      wxwidgets:png=${wx_png}
      wxwidgets:jpeg=${wx_jpeg}
      wxwidgets:tiff=${wx_tiff}
      wxwidgets:secretstore=False
      wxwidgets:opengl=False
      wxwidgets:propgrid=False
      wxwidgets:ribbon=False
      wxwidgets:richtext=False
      wxwidgets:stc=False
      wxwidgets:webview=False
      wxwidgets:help=False
      wxwidgets:html_help=False
      wxwidgets:fs_inet=False
      wxwidgets:protocol=False
      # Building with SIMD requires a huge number of build dependencies
      # Probably this will be enabled in the future
      libjpeg-turbo:SIMD=False
)

add_conan_lib(
   libmp3lame
   libmp3lame/3.100
   REQUIRED
   INTERFACE_NAME libmp3lame::libmp3lame
   PKG_CONFIG "lame >= 3.100"
   ALLOW_FIND_PACKAGE
)

add_conan_lib(
   libid3tag
   libid3tag/0.15.2b@audacity/stable
   OPTION_NAME libid3tag
   PKG_CONFIG "id3tag >= 0.15.0b" # Ubuntu has broken PC file
   CONAN_OPTIONS
      libid3tag:zlib=${id3tag_zlib}
)

add_conan_lib(
   libmad
   libmad/0.15.2b@audacity/stable
   OPTION_NAME libmad
   PKG_CONFIG "mad >= 0.15.0b" # Ubuntu has broken PC file
)

add_conan_lib(
   Ogg
   ogg/1.3.4
   OPTION_NAME libogg
   ALLOW_FIND_PACKAGE
   CONAN_OPTIONS
      ogg:shared=True
)

add_conan_lib(
   FLAC
   flac/1.3.3
   OPTION_NAME libflac
   ALLOW_FIND_PACKAGE
   CONAN_OPTIONS
      flac:shared=True
)

add_conan_lib(
   Opus
   opus/1.3.1
   OPTION_NAME libopus
   ALLOW_FIND_PACKAGE
   CONAN_OPTIONS
      opus:shared=True
)

add_conan_lib(
   Vorbis
   vorbis/1.3.7
   OPTION_NAME libvorbis
   ALLOW_FIND_PACKAGE
   CONAN_OPTIONS
      vorbis:shared=True
)

add_conan_lib(
   SndFile
   libsndfile/1.0.31
   OPTION_NAME libsndfile
   ALLOW_FIND_PACKAGE
   CONAN_OPTIONS
      libsndfile:shared=True
)

# Allow building with ASIO for windows users
cmake_dependent_option(
   ${_OPT}has_asio_support
   "Build Audacity with ASIO support"
   False
   "WIN32 AND IS_64BIT"
   False
)

# Conan expects Python boolean, CMake will convert values to
# ON/OFF

if( ${_OPT}has_asio_support )
   set( portaudio_enable_asio "portaudio:with_asio=True" )
endif()

add_conan_lib(
   PortAudio
   portaudio/19.7.0
   OPTION_NAME portaudio
   ALLOW_FIND_PACKAGE
   INTERFACE_NAME PortAudio::PortAudio
   CONAN_OPTIONS
      portaudio:shared=True
      # Audacity doesn't support WDM/KS (yet)
      portaudio:with_wdmks=False
      ${portaudio_enable_asio}
)

if( ${_OPT}has_networking )
   if( CMAKE_SYSTEM_NAME MATCHES "Darwin" )
      set( curl_ssl "darwinssl" )
   elseif( CMAKE_SYSTEM_NAME MATCHES "Windows" )
      set( curl_ssl "schannel")
   else()
      set ( curl_ssl "openssl" )
   endif ()

   add_conan_lib(
      ThreadPool
      threadpool/20140926
      REQUIRED
      ALWAYS_ALLOW_CONAN_FALLBACK
   )

   add_conan_lib(
      CURL
      libcurl/7.75.0
      REQUIRED
      OPTION_NAME curl
      PKG_CONFIG "libcurl >= 7.68.0"
      INTERFACE_NAME CURL::libcurl
      FIND_PACKAGE_OPTIONS
      CONAN_OPTIONS
         libcurl:with_ssl=${curl_ssl}
         libcurl:shared=True
   )

endif()

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows")
   add_conan_lib(
      libuuid
      libuuid/1.0.3
      REQUIRED
      OPTION_NAME uuid
      PKG_CONFIG "uuid"
      INTERFACE_NAME libuuid::libuuid
   )
endif()

if( ${_OPT}has_sentry_reporting )
   add_conan_lib(
      RapidJSON
      rapidjson/1.1.0
      REQUIRED
   )
endif()

add_conan_lib(
   PortMidi
   portmidi/r234
   OPTION_NAME midi
   ALLOW_FIND_PACKAGE
   CONAN_OPTIONS
      portmidi:shared=True
)

set_conan_vars_to_parent()

# Required libraries
#
#       directory          option      symbol      req   chk   version
addlib( sqlite             sqlite      SQLITE      YES   YES   "sqlite3 >= 3.31.1" )

# Optional libraries
#
addlib( libvamp            vamp        VAMP        NO    YES   "vamp >= 2.5" "vamp-hostsdk >= 2.5" )
addlib( lv2                lv2         LV2         NO    YES   "lilv-0 >= 0.24.6" "lv2 >= 1.16.0" "serd-0 >= 0.30.2" "sord-0 >= 0.16.4" "sratom-0 >= 0.6.4" "suil-0 >= 0.10.6" )

#addlib( portmidi           midi        MIDI        NO    YES   "PortMidi" )

if (NOT USE_MIDI AND
   "EXPERIMENTAL_MIDI_OUT" IN_LIST EXPERIMENTAL_OPTIONS_LIST )
   message(FATAL_ERROR "EXPERIMENTAL_MIDI_OUT requires USE_MIDI")
endif ()

addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
addlib( libsbsms           sbsms       SBSMS       NO    YES   "sbsms >= 2.2.0" )
addlib( soundtouch         soundtouch  SOUNDTOUCH  NO    YES   "soundtouch >= 1.7.1" )
addlib( twolame            twolame     LIBTWOLAME  NO    YES   "twolame >= 0.3.13" )
