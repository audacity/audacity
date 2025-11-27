#[[
A library defining the top Project object, which is little more than a
container for attached objects built by registered factories, and an event
handler.

Also a global array of existing projects.

Also ProjectStatus, an abstraction of the status bar, just holding some strings
and formatter functions, and emitting events when changed.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   Project.cpp
   Project.h
   ProjectFormatVersion.cpp
   ProjectFormatVersion.h
   ProjectStatus.cpp
   ProjectStatus.h
)
set( LIBRARIES
   au3-registries-interface
   au3-xml-interface
)
audacity_library( au3-project "${SOURCES}" "${LIBRARIES}"
   "" ""
)
