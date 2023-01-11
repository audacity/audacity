#[[
A non-modal dialog computes rms levels of selections, and the difference
between foreground and background
]]
list( APPEND SOURCES
   Contrast.cpp
   Contrast.h
)
audacity_module( mod-contrast "${SOURCES}" "Audacity"
   "" "" )
