# Required libraries
#
#       directory          option      symbol      req   chk   version


# Optional libraries
#
addlib( lv2                lv2         LV2         NO    YES   "lilv-0 >= 0.24.6" "lv2 >= 1.16.0" "serd-0 >= 0.30.2" "sord-0 >= 0.16.4" "sratom-0 >= 0.6.4" "suil-0 >= 0.10.6" )

#addlib( portmidi           midi        MIDI        NO    YES   "PortMidi" )

if (NOT USE_MIDI AND
   "EXPERIMENTAL_MIDI_OUT" IN_LIST EXPERIMENTAL_OPTIONS_LIST )
   message(FATAL_ERROR "EXPERIMENTAL_MIDI_OUT requires USE_MIDI")
endif ()

addlib( twolame            twolame     LIBTWOLAME  NO    YES   "twolame >= 0.3.13" )
addlib( pffft              pffft       PFFFT       YES   NO    "" )
