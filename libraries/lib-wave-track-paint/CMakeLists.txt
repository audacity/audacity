#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   PixelSampleMapper.cpp
   PixelSampleMapper.h
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
