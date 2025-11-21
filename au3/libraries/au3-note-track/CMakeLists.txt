#[[
A track that holds MIDI data.  Playback of MIDI.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

if( NOT TARGET portsmf )
   addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
endif()

set( SOURCES
   MIDIPlay.cpp
   MIDIPlay.h
   NoteTrack.cpp
   NoteTrack.h
   WrapAllegro.h
)
set( LIBRARIES
   lib-audio-io-interface
   lib-playable-track-interface
   portmidi::portmidi
   portsmf
)
audacity_library( lib-note-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
