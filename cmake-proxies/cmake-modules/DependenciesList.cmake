# conan_package_options variable can be used to pass additional options to the conan install command.

audacity_find_package(ZLIB REQUIRED)
audacity_find_package(EXPAT REQUIRED)
audacity_find_package(PNG QUIET CONAN_PACKAGE_NAME libpng)
audacity_find_package(JPEG QUIET CONAN_PACKAGE_NAME libjpeg-turbo)

audacity_find_package(wxWidgets REQUIRED FIND_PACKAGE_OPTIONS COMPONENTS adv base core html qa xml net)

audacity_find_package(libmp3lame REQUIRED)

audacity_find_package(mpg123 OPTION_NAME libmpg123)

if( NOT ${_OPT}use_libmpg123 STREQUAL "off" )
   # If we are building against libmpg123, we need to drop
   # the previos configuration, which may used libmad
   set( USE_LIBMAD OFF CACHE INTERNAL "" FORCE )
   set( ${_OPT}use_libmad "off" )
else()
   audacity_find_package(libmad)
endif()

audacity_find_package(libid3tag)

audacity_find_package(WavPack)
audacity_find_package(Ogg OPTION_NAME libogg)
audacity_find_package(FLAC OPTION_NAME libflac)
audacity_find_package(Opus OPTION_NAME libopus)
audacity_find_package(Vorbis OPTION_NAME libvorbis)
audacity_find_package(SndFile CONAN_PACKAGE_NAME libsndfile OPTION_NAME libsndfile)

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
   list(APPEND conan_package_options "use_asio=True")
endif()

if(UNIX AND NOT APPLE)
   find_package(JACK QUIET)

   if( JACK_FOUND )
      list(APPEND conan_package_options  "use_jack=True")
   endif()
endif()

audacity_find_package(PortAudio REQUIRED)

if( ${_OPT}has_networking )
   audacity_find_package(ThreadPool REQUIRED)
   audacity_find_package(CURL REQUIRED CONAN_PACKAGE_NAME libcurl)
endif()

audacity_find_package(RapidJSON REQUIRED)

audacity_find_package(PortMidi OPTION_NAME midi)

if(${_OPT}has_crashreports)
   audacity_find_package(breakpad REQUIRED)
   if(${_OPT}crashreport_backend STREQUAL crashpad)
      audacity_find_package(crashpad REQUIRED)
   endif()
endif()

if(${_OPT}has_vst3)
   audacity_find_package(vst3sdk REQUIRED)
endif()

if( ${_OPT}has_tests )
   audacity_find_package(Catch2 REQUIRED)
endif()

if(NOT CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows")
   find_package(libuuid REQUIRED)
endif()
