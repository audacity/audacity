# Allow user to globally set the library preference
cmd_option( ${_OPT}lib_preference
            "Library preference [system (if available), local]"
            "system"
            STRINGS "system" "local"
)

# Required libraries
#
#       directory          option      symbol      req   chk   version
addlib( wxWidgets          wxwidgets   WX          YES   NO    "" ) # must be first
addlib( expat              expat       EXPAT       YES   YES   "expat >= 2.1.0" )
addlib( lame               lame        LAME        YES   YES   "lame >= 3.100" )
addlib( libsndfile         sndfile     SNDFILE     YES   YES   "sndfile >= 1.0.28" )
addlib( libsoxr            soxr        SOXR        YES   YES   "soxr >= 0.1.1" )
addlib( portaudio-v19      portaudio   PORTAUDIO   YES   YES   "" )
addlib( sqlite             sqlite      SQLITE      YES   YES   "sqlite3 >= 3.32.0" )

# Optional libraries
#
#       directory          option      symbol      req   chk   version
addlib( ffmpeg             ffmpeg      FFMPEG      NO    NO    "libavcodec >= 51.53" "libavformat >= 52.12" "libavutil >= 52.66" )
addlib( libid3tag          id3tag      LIBID3TAG   NO    YES   "id3tag >= 0.15.1b" )
addlib( libmad             mad         LIBMAD      NO    YES   "mad >= 0.15.1b" )
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

