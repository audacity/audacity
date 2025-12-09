#[[
A library of utilities relating generally to files and file names, including
an exception type for file errors.

Also the definition of certain significant file paths, such as the directory
for temporary projects.

Also the global logger which can save to a file.
]]

set( TARGET au3-files )

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
   PlatformCompatibility.h
   TempDirectory.cpp
   TempDirectory.h
   wxFileNameWrapper.h
)
# Add platform-specific PlatformCompatibility implementations
if(OS_IS_WIN)
    list(APPEND SOURCES PlatformCompatibilityWindows.cpp)
elseif(OS_IS_LIN)
    list(APPEND SOURCES PlatformCompatibilityLinux.cpp)
elseif(OS_IS_MAC)
    list(APPEND SOURCES PlatformCompatibilityMacOS.cpp)
endif()

set( LIBRARIES
   au3-exceptions-interface
   au3-preferences-interface
   wxBase
)
set(DEFINES
   PRIVATE
      -DAUDACITY_VERSION=${AUDACITY_VERSION}
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "")
