#[[
A track that holds MIDI data.  Playback of MIDI.
]]

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
