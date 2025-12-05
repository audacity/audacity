#[[
A library responsible for custom rendering in Audacity
]]

set( TARGET au3-graphics )

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
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
