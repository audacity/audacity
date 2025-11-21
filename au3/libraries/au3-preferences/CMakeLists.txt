#[[
Classes managing settings that persist between runs of the program.

Settings objects for various types (boolean, numbers, strings, enumerations)
that package a preference key path and a default value (so that literals are
not duplicated in widely separated parts) and cache values in memory.

PreferenceListener which is a callback notified of certain changes of
preferences.

PreferenceInitializer is a callback for the reinitialization of preferences.

FileConfig decorates wxFileConfig, with a member function to
alert the user of failure to initialize it (as when the file is read-only).
]]

set( SOURCES
   BasicSettings.cpp
   BasicSettings.h
   Prefs.cpp
   Prefs.h
)
set( LIBRARIES 
   lib-basic-ui-interface
   lib-components-interface
)
audacity_library( lib-preferences "${SOURCES}" "${LIBRARIES}"
   "" ""
)
