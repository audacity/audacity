#[[
Facilities for defining and visiting the heterogenously typed parameters of an
effect, or command
]]

set( SOURCES
   ShuttleAutomation.cpp
   ShuttleAutomation.h
   ShuttlePrefs.cpp
   ShuttlePrefs.h
   WrappedType.cpp
   WrappedType.h
)
set( LIBRARIES
   au3-preferences-interface
)
audacity_library( au3-command-parameters "${SOURCES}" "${LIBRARIES}"
   "" ""
)

