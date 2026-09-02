#[[
Facilities for defining and visiting the heterogenously typed parameters of an
effect, or command
]]

set( TARGET au3-command-parameters )

set( SOURCES
   ShuttleAutomation.cpp
   ShuttleAutomation.h
   ShuttlePrefs.cpp
   ShuttlePrefs.h
   WrappedType.cpp
   WrappedType.h
)
set( LIBRARIES
PUBLIC
   au3-components
PRIVATE
   au3-preferences
   au3-strings
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)

