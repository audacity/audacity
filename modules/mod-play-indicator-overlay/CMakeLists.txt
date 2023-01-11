list( APPEND SOURCES
   PlayIndicatorOverlay.cpp
   PlayIndicatorOverlay.h
)
audacity_module( mod-play-indicator-overlay "${SOURCES}"
   "Audacity"
   "" "" )
