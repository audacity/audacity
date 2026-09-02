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
   au3-graphics
   au3-math
   au3-utility
PRIVATE
   au3-mixer
   au3-screen-geometry
   au3-strings
   au3-track
   au3-wave-track
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
