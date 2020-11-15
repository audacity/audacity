#[[
Adds the Keyboard page to preferences, supplying a user interface to change
key bindings to commands
]]

set( SOURCES
   KeyConfigPrefs.cpp
   KeyConfigPrefs.h
   KeyView.cpp
   KeyView.h
)
audacity_module( mod-keyboard-preferences "${SOURCES}" "Audacity"
   "" "" )
