#[[
Adds all user interface related to MIDI tracks (called NoteTrack in the code),
including preference page and menu items.

Draws colored bars representing MIDI notes and a vertical ruler as a piano
keyboard.  Adds some mouse interactions.

If this module is disabled but MIDI tracks exist in the file, they may still
be importable and playable, but may display as a plain gray rectangle with no
mouse interaction.
]]

if ( NOT TARGET portsmf )
   addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
endif()

set( SOURCES
   MidiIOPrefs.cpp
   MidiIOPrefs.h
   NoteTrackAffordanceControls.cpp
   NoteTrackAffordanceControls.h
   NoteTrackButtonHandle.cpp
   NoteTrackButtonHandle.h
   NoteTrackControls.cpp
   NoteTrackControls.h
   NoteTrackDisplayData.cpp
   NoteTrackDisplayData.h
   NoteTrackMenuItems.cpp
   NoteTrackShifter.cpp
   NoteTrackSliderHandles.cpp
   NoteTrackSliderHandles.h
   NoteTrackVRulerControls.cpp
   NoteTrackVRulerControls.h
   NoteTrackVZoomHandle.cpp
   NoteTrackVZoomHandle.h
   NoteTrackView.cpp
   NoteTrackView.h
   StretchHandle.cpp
   StretchHandle.h
)
set (LIBRARIES
   Audacity
   portsmf
)
audacity_module( mod-note-track-ui "${SOURCES}" "${LIBRARIES}"
   "" "" )
