#[[
A library handling project persistency, and supplying concrete SampleBlock
and SampleBlockFactory subclasses so that au3-wave-track is usable.

All usage of Sqlite is encapsulated in this library.
]]

set( TARGET au3-project-file-io )

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
PUBLIC
   au3-preferences
   au3-registries
   au3-strings
   au3-utility
   au3-xml
PRIVATE
   au3-basic-ui
   au3-files
   au3-math
   au3-network-manager
   au3-project
   au3-project-history
   au3-realtime-effects
   au3-sentry-reporting
   au3-sqlite-helpers
   au3-stretching-sequence
   au3-string-utils
   au3-track
   au3-transactions
   au3-wave-track
   # Only this library needs sqlite, so make the dependency private
   sqlite
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   ""
   ""
)
