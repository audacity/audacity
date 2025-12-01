#[[
A library to hold a registry of image and color identifiers, and multiple themes
associating data with those identifiers.
]]

set( TARGET au3-theme )

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
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
