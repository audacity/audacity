add_conan_lib(
   ZLIB
   zlib/1.2.11
   REQUIRED
   PGK_CONFIG "zlib >= 1.2.11"
   INTERFACE_NAME ZLIB::ZLIB
   OPTION_NAME zlib
   CONAN_OPTIONS 
      zlib:shared=True
)

add_conan_lib(
   expat
   expat/2.2.9
   REQUIRED
   PGK_CONFIG "expat >= 2.1.0"
   CONAN_OPTIONS 
      expat:shared=True
)

set(wx_zlib "zlib")
set(wx_png "libpng")

if (${_OPT}use_zlib STREQUAL "system")
   set(wx_zlib "sys")
   # To prevent linking conflicts - we need to use system libpng as well.
   # wxWdigets will attempt to resolve it using find_package
   set(wx_png "sys")
endif()

set(wx_expat "expat")

if (${_OPT}use_expat STREQUAL "system")
   set(wx_expat "sys")
endif()

add_conan_lib( 
   wxWidgets 
   wxwidgets/3.1.3-audacity 
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
      wxwidgets:tiff=off
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
)

add_conan_lib(
   libmp3lame
   libmp3lame/3.100
   REQUIRED
   INTERFACE_NAME libmp3lame::libmp3lame
   PGK_CONFIG "lame >= 3.100"
)

add_conan_lib(
   libid3tag
   libid3tag/0.15.1b
   OPTION_NAME id3tag
   HAS_ONLY_DEBUG_RELEASE
   PGK_CONFIG "id3tag >= 0.15.1b"
)

add_conan_lib(
   libmad
   libmad/0.15.1b
   OPTION_NAME mad
   HAS_ONLY_DEBUG_RELEASE
   PGK_CONFIG "mad >= 0.15.1b"
)

set_conan_vars_to_parent()

# Required libraries
#
#       directory          option      symbol      req   chk   version
addlib( libsndfile         sndfile     SNDFILE     YES   YES   "sndfile >= 1.0.28" )
addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )
addlib( portaudio-v19      portaudio   PORTAUDIO   YES   YES   "" )
addlib( sqlite             sqlite      SQLITE      YES   YES   "sqlite3 >= 3.32.0" )

# Optional libraries
#
#       directory          option      symbol      req   chk   version
addlib( ffmpeg             ffmpeg      FFMPEG      NO    NO    "libavcodec >= 51.53" "libavformat >= 52.12" "libavutil >= 52.66" )
addlib( libnyquist         nyquist     NYQUIST     NO    YES   "" )
addlib( libvamp            vamp        VAMP        NO    YES   "vamp >= 2.5" "vamp-hostsdk >= 2.5" )
addlib( libogg             ogg         LIBOGG      NO    YES   "ogg >= 1.3.1" )
addlib( libvorbis          vorbis      LIBVORBIS   NO    YES   "vorbis >= 1.3.3" "vorbisenc >= 1.3.3" "vorbisfile >= 1.3.3" )
addlib( libflac            flac        LIBFLAC     NO    YES   "flac >= 1.3.1" "flac++ >= 1.3.1" )
addlib( lv2                lv2         LV2         NO    YES   "lilv-0 >= 0.24.6" "lv2 >= 1.16.0" "serd-0 >= 0.30.2" "sord-0 >= 0.16.4" "sratom-0 >= 0.6.4" "suil-0 >= 0.10.6" )

addlib( portmidi           midi        MIDI        NO    YES   "portmidi >= 0.1" )
if (NOT USE_MIDI AND
   "EXPERIMENTAL_MIDI_OUT" IN_LIST EXPERIMENTAL_OPTIONS_LIST )
   message(FATAL_ERROR "EXPERIMENTAL_MIDI_OUT requires USE_MIDI")
endif ()


addlib( portmixer          portmixer   PORTMIXER   NO    YES   "" )
if (NOT USE_PORTMIXER AND
   "EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT" IN_LIST
      EXPERIMENTAL_OPTIONS_LIST )
   message(FATAL_ERROR "EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT requires USE_PORTMIXER")
endif ()

addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
addlib( libsbsms           sbsms       SBSMS       NO    YES   "sbsms >= 2.2.0" )
addlib( soundtouch         soundtouch  SOUNDTOUCH  NO    YES   "soundtouch >= 1.7.1" )
addlib( twolame            twolame     LIBTWOLAME  NO    YES   "twolame >= 0.3.13" )
