#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   FrameStatistics.cpp
   FrameStatistics.h

   GraphicsDataCache.cpp
   GraphicsDataCache.h

   GraphicsObjectCache.h

   PixelSampleMapper.cpp
   PixelSampleMapper.h

   graphics/Brush.cpp
   graphics/Brush.h
   graphics/Color.cpp
   graphics/Color.h
   graphics/FontInfo.cpp
   graphics/FontInfo.h
   graphics/Painter.cpp
   graphics/Painter.h
   graphics/Pen.cpp
   graphics/Pen.h
   graphics/PathTriangulation.cpp
   graphics/PathTriangulation.h
   graphics/Point.h
   graphics/Rect.h
   graphics/RendererID.cpp
   graphics/RendererID.h
   graphics/Size.h
   graphics/Transform.h

   waveform/WaveClipPainter.cpp
   waveform/WaveClipPainter.h
   waveform/WaveData.cpp
   waveform/WaveData.h
   waveform/WaveDataCache.cpp
   waveform/WaveDataCache.h
   waveform/WavePaintParameters.cpp
   waveform/WavePaintParameters.h
)
set( LIBRARIES
   PUBLIC
      lib-utility-interface
   PRIVATE
      lib-math-interface
      lib-screen-geometry-interface
      wxwidgets::base
)
audacity_library( lib-graphics "${SOURCES}" "${LIBRARIES}"
   "" ""
)
