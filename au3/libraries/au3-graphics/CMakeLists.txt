#[[
A library responsible for custom rendering in Audacity
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
      au3-utility-interface
)
audacity_library( au3-graphics "${SOURCES}" "${LIBRARIES}"
   "" ""
)
