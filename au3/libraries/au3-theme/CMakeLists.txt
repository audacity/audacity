#[[
A library to hold a registry of image and color identifiers, and multiple themes
associating data with those identifiers.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AColor.cpp
   AColor.h
   AColorResources.h
   AllThemeResources.cpp
   AllThemeResources.h
   ImageManipulation.cpp
   ImageManipulation.h
   MacroMagic.h
   Theme.cpp
   Theme.h
)
set( LIBRARIES
   au3-files-interface
   wxwidgets::wxwidgets
 )
audacity_library( au3-theme "${SOURCES}" "${LIBRARIES}"
   "" ""
)
