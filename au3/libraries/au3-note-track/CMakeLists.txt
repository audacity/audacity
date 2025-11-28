#[[
A track that holds MIDI data.  Playback of MIDI.
]]

set( SOURCES
   MIDIPlay.cpp
   MIDIPlay.h
   NoteTrack.cpp
   NoteTrack.h
   WrapAllegro.h
)
set( LIBRARIES
   au3-audio-io-interface
   au3-playable-track-interface
   portmidi::portmidi
   portsmf
)
audacity_library( au3-note-track "${SOURCES}" "${LIBRARIES}"
   "" ""
)
