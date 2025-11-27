#[[
A library of utilities relating generally to files and file names, including
an exception type for file errors.

Also the definition of certain significant file paths, such as the directory
for temporary projects.

Also the global logger which can save to a file.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AudacityLogger.cpp
   AudacityLogger.h
   FileException.cpp
   FileException.h
   FileIO.cpp
   FileIO.h
   FileNames.cpp
   FileNames.h
   PathList.cpp
   PathList.h
   PlatformCompatibility.cpp
   PlatformCompatibility.h
   PlatformCompatibilityWX.cpp # requires a wxWidgets app ; alternative impl needed if not using wxWidgets.
   TempDirectory.cpp
   TempDirectory.h
   wxFileNameWrapper.h
)
set( LIBRARIES
   au3-exceptions-interface
   au3-preferences-interface
)
set(DEFINES
   PRIVATE
      -DAUDACITY_VERSION=${AUDACITY_VERSION}
)
audacity_library( au3-files "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "")
