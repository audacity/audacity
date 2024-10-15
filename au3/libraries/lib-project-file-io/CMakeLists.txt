#[[
A library handling project persistency, and supplying concrete SampleBlock
and SampleBlockFactory subclasses so that lib-wave-track is usable.

All usage of Sqlite is encapsulated in this library.
]]

addlib( sqlite             sqlite      SQLITE      YES   YES   "sqlite3 >= 3.31.1" )

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
   lib-wave-track-interface
)

list( APPEND LIBRARIES
   # If Sentry reporting is enabled, this library depends on
   # lib-network-manager-interface
   # If Sentry reporting is disabled, an INTERFACE library
   # will be defined
   lib-sentry-reporting-interface
)

# Only this library needs sqlite, so make the dependency private
list( APPEND LIBRARIES
   PRIVATE
      lib-sqlite-helpers-interface
)

audacity_library( lib-project-file-io "${SOURCES}" "${LIBRARIES}"
   ""
   ""
)
