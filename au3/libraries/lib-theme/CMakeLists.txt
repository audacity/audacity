#[[
A library to hold a registry of image and color identifiers, and multiple themes
associating data with those identifiers.
]]

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
   lib-files-interface
   wxwidgets::wxwidgets
 )
audacity_library( lib-theme "${SOURCES}" "${LIBRARIES}"
   "" ""
)
