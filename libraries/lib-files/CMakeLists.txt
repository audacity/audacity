#[[
A library of utilities relating generally to files and file names, including
an exception type for file errors.

Also the definition of certain significant file paths, such as the directory
for temporary projects.

Also the global logger which can save to a file.
]]

if(WIN32)
   set( PlatformCompatibilityOsSpecific PlatformCompatibilityWindows )
elseif(APPLE)
   set( PlatformCompatibilityOsSpecific PlatformCompatibilityMacOS )
else()
   set( PlatformCompatibilityOsSpecific PlatformCompatibilityLinux )
endif()

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
   ${PlatformCompatibilityOsSpecific}.cpp
   PlatformCompatibility.cpp
   PlatformCompatibility.h
   PlatformCompatibilityWX.cpp # requires a wxWidgets app ; alternative impl needed if not using wxWidgets.
   TempDirectory.cpp
   TempDirectory.h
   wxFileNameWrapper.h
)
set( LIBRARIES
   lib-exceptions-interface
   lib-preferences-interface
)
set(DEFINES
   PRIVATE
      -DAUDACITY_VERSION=${AUDACITY_VERSION}
)
audacity_library( lib-files "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "")
