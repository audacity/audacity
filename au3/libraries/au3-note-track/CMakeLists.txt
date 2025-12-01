#[[
A track that holds MIDI data.  Playback of MIDI.
]]

set( TARGET au3-note-track )

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
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
