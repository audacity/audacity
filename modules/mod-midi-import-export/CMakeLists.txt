#[[
Adds MIDI import and export, using the portsmf library
]]

if ( NOT TARGET portsmf )
   addlib( portsmf            portsmf     PORTSMF     NO    YES   "portSMF >= 0.1" )
endif()

set( SOURCES
   ExportMIDI.cpp
   ImportMIDI.cpp
)
set( LIBRARIES
   Audacity
   portsmf
)
audacity_module( mod-midi-import-export "${SOURCES}" "${LIBRARIES}"
   "" "" )
