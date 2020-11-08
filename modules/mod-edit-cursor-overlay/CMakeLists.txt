list( APPEND SOURCES
   EditCursorOverlay.cpp
   EditCursorOverlay.h
)
audacity_module( mod-edit-cursor-overlay "${SOURCES}"
   "Audacity"
   "" "" )
