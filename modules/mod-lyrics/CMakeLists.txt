#[[
The famous Karaoke dialog.

A non-modal dialog with a bouncing ball animates the synchronization of text
labels with audio playback position.

A leftover piece of a voice recognition experiment by Audacity co-founder
Dominic Mazzoni.
]]

list( APPEND SOURCES
   Lyrics.cpp
   Lyrics.h
   LyricsWindow.cpp
   LyricsWindow.h
)
audacity_module( mod-lyrics "${SOURCES}" "Audacity"
   "" "" )
