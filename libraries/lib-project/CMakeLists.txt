#[[
A library defining the top Project object, which is little more than a
container for attached objects built by registered factories, and an event
handler.

Also a global array of existing projects.

Also ProjectStatus, an abstraction of the status bar, just holding some strings
and formatter functions, and emitting events when changed.
]]

set( SOURCES
   Project.cpp
   Project.h
   ProjectFormatVersion.cpp
   ProjectFormatVersion.h
   ProjectStatus.cpp
   ProjectStatus.h
)
set( LIBRARIES
   lib-registries-interface
   lib-xml-interface
)
audacity_library( lib-project "${SOURCES}" "${LIBRARIES}"
   "" ""
)
