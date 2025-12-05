#[[
A library responsible for custom rendering in Audacity
]]

set( TARGET au3-wave-track-paint )

set( SOURCES
   GraphicsDataCache.cpp
   GraphicsDataCache.h

   PixelSampleMapper.cpp
   PixelSampleMapper.h

   waveform/WaveBitmapCache.cpp
   waveform/WaveBitmapCache.h
   waveform/WaveData.cpp
   waveform/WaveData.h
   waveform/WaveDataCache.cpp
   waveform/WaveDataCache.h
   waveform/WavePaintParameters.cpp
   waveform/WavePaintParameters.h
)
set( LIBRARIES
   PUBLIC
      au3-utility-interface
      wxBase
   PRIVATE
      au3-math-interface
      au3-screen-geometry-interface
      au3-track-interface
      au3-mixer-interface
      au3-graphics-interface
      au3-wave-track-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
