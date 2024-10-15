#[[
A library responsible for custom rendering in Audacity
]]

set( SOURCES
   FrameStatistics.cpp
   FrameStatistics.h

   graphics/Color.h
   graphics/Point.h
   graphics/Rect.h
   graphics/Size.h
)
set( LIBRARIES
   PRIVATE
      lib-utility-interface
)
audacity_library( lib-graphics "${SOURCES}" "${LIBRARIES}"
   "" ""
)
