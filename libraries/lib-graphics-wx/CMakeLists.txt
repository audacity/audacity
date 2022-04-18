#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   graphics/WXColor.cpp
   graphics/WXColor.h
   graphics/WXGraphicsContextPainter.cpp
   graphics/WXGraphicsContextPainter.h
   graphics/WXPainterFactory.cpp
   graphics/WXPainterFactory.h
   graphics/WXPainterUtils.cpp
   graphics/WXPainterUtils.h

   waveform/WaveBitmapCache.cpp
   waveform/WaveBitmapCache.h
)
set( LIBRARIES
   PUBLIC
      lib-graphics-interface
   PRIVATE
      lib-math-interface
      lib-screen-geometry-interface
      lib-track-interface
      wxwidgets::base
      wxwidgets::core
)
audacity_library( lib-graphics-wx "${SOURCES}" "${LIBRARIES}"
   "" ""
)
