#[[
A library holding image and color data for the four built-in themes
]]#

set( SOURCES
   ClassicThemeAsCeeCode.cpp
   ClassicThemeAsCeeCode.h
   DarkThemeAsCeeCode.cpp
   DarkThemeAsCeeCode.h
   HighContrastThemeAsCeeCode.cpp
   HighContrastThemeAsCeeCode.h
   LightThemeAsCeeCode.cpp
   LightThemeAsCeeCode.h
   LoadThemeResources.h
   LoadThemeResources.cpp
)
set( LIBRARIES
   lib-theme-interface
   PRIVATE
   wxwidgets::wxwidgets
 )
audacity_library( lib-theme-resources "${SOURCES}" "${LIBRARIES}"
   "" ""
)
