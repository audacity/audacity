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
   lib-preferences-interface
   wxBase
)
audacity_library( lib-command-parameters "${SOURCES}" "${LIBRARIES}"
   "" ""
)

