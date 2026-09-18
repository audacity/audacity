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
PUBLIC
   au3-audio-io
   au3-mixer
   au3-playable-track
   au3-preferences
   au3-utility
PRIVATE
   au3-audio-devices
   au3-basic-ui
   au3-exceptions
   au3-project
   au3-strings
   au3-track
   portmidi::portmidi
   portsmf
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
