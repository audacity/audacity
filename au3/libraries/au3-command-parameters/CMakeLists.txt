#[[
Facilities for defining and visiting the heterogenously typed parameters of an
effect, or command
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
)
audacity_library( lib-command-parameters "${SOURCES}" "${LIBRARIES}"
   "" ""
)

