#[[
A library holding the global settings for preferred sample rate, format, and
dB scale cutoff.   Also defines the object that holds the per-project sample
rate setting.

This library depends on lib-audio-devices, because available devices may be
queried to decide the default sample rate.
]]

set( SOURCES
   Decibels.cpp
   Decibels.h
   ProjectRate.cpp
   ProjectRate.h
   QualitySettings.cpp
   QualitySettings.h
)
set( LIBRARIES
   lib-project-interface
   lib-audio-devices-interface
   lib-math-interface
)
audacity_library( lib-project-rate "${SOURCES}" "${LIBRARIES}"
   "" ""
)
