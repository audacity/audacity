#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   graphics/fonts/Font.cpp
   graphics/fonts/Font.h
   graphics/fonts/FontFace.cpp
   graphics/fonts/FontFace.h
   graphics/fonts/FontLibrary.cpp
   graphics/fonts/FontLibrary.h
   graphics/fonts/FontProvider.cpp
   graphics/fonts/FontProvider.h
   graphics/fonts/FontRenderer.cpp
   graphics/fonts/FontRenderer.h
   graphics/fonts/TextLayout.cpp
   graphics/fonts/TextLayout.h

   $<$<PLATFORM_ID:Windows>:graphics/fonts/platforms/windows/WindowsFontProvider.cpp>
   $<$<PLATFORM_ID:Windows>:graphics/fonts/platforms/windows/WindowsFontProvider.h>

   $<$<PLATFORM_ID:Darwin>:graphics/fonts/platforms/macos/MacosFontProvider.mm>
   $<$<PLATFORM_ID:Darwin>:graphics/fonts/platforms/macos/MacosFontProvider.h>

   $<$<NOT:$<PLATFORM_ID:Windows,Darwin>>:graphics/fonts/platforms/linux/LinuxFontProvider.cpp>
   $<$<NOT:$<PLATFORM_ID:Windows,Darwin>>:graphics/fonts/platforms/linux/LinuxFontProvider.h>
)

if( NOT CMAKE_SYSTEM_NAME MATCHES "Darwin|Windows" )
   find_package(Fontconfig REQUIRED)
endif()

set( LIBRARIES
   PUBLIC
      lib-graphics-interface
   PRIVATE
      lib-math-interface
      lib-screen-geometry-interface
      Freetype::Freetype
      harfbuzz::harfbuzz
      wxwidgets::base
      $<$<NOT:$<PLATFORM_ID:Windows,Darwin>>:Fontconfig::Fontconfig>
)

audacity_library( lib-graphics-fonts "${SOURCES}" "${LIBRARIES}"
   "" ""
)
