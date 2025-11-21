#[[
A library responsible for custom rendering in Audacity
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
      lib-utility-interface
   PRIVATE
      lib-math-interface
      lib-screen-geometry-interface
      lib-track-interface
      lib-mixer-interface
      lib-graphics-interface
      lib-wave-track-interface
)
audacity_library( lib-wave-track-paint "${SOURCES}" "${LIBRARIES}"
   "" ""
)
