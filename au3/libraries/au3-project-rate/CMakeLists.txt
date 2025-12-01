#[[
A library holding the global settings for preferred sample rate, format, and
dB scale cutoff.   Also defines the object that holds the per-project sample
rate setting.

This library depends on au3-audio-devices, because available devices may be
queried to decide the default sample rate.
]]

set( TARGET au3-project-rate )

set( SOURCES
   Decibels.cpp
   Decibels.h
   ProjectRate.cpp
   ProjectRate.h
   QualitySettings.cpp
   QualitySettings.h
)
set( LIBRARIES
   au3-project-interface
   au3-audio-devices-interface
   au3-math-interface
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
