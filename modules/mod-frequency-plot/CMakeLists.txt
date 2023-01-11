#[[
A non-modal dialog computes spectral plots for selected audio
]]
list( APPEND SOURCES
   FreqWindow.cpp
   FreqWindow.h
)
audacity_module( mod-frequency-plot "${SOURCES}" "Audacity"
   "" "" )
