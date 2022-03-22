#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   FrameStatistics.cpp
   FrameStatistics.h

   GraphicsDataCache.cpp
   GraphicsDataCache.h

   PixelSampleMapper.cpp
   PixelSampleMapper.h

   graphics/Color.cpp
   graphics/Color.h

   waveform/WaveData.cpp
   waveform/WaveData.h
   waveform/WaveDataCache.cpp
   waveform/WaveDataCache.h
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
