#[[
A library handling project persistency, and supplying concrete SampleBlock
and SampleBlockFactory subclasses so that au3-wave-track is usable.

All usage of Sqlite is encapsulated in this library.
]]

set( SOURCES
   ActiveProjects.cpp
   ActiveProjects.h
   DBConnection.cpp
   DBConnection.h
   ProjectFileIOExtension.cpp
   ProjectFileIOExtension.h
   ProjectFileIO.cpp
   ProjectFileIO.h
   ProjectSerializer.cpp
   ProjectSerializer.h
   SqliteSampleBlock.cpp
)

set( LIBRARIES
   au3-wave-track-interface
)

list( APPEND LIBRARIES
   # If Sentry reporting is enabled, this library depends on
   # au3-network-manager-interface
   # If Sentry reporting is disabled, an INTERFACE library
   # will be defined
   au3-sentry-reporting-interface
)

# Only this library needs sqlite, so make the dependency private
list( APPEND LIBRARIES
   PRIVATE
      au3-sqlite-helpers-interface
)

audacity_library( au3-project-file-io "${SOURCES}" "${LIBRARIES}"
   ""
   ""
)
