#[[
UI-agnostic library for WaveTrack settings.
]]

set( TARGET au3-wave-track-settings )

set( SOURCES
   WaveformSettings.cpp
   WaveformSettings.h
)
set( LIBRARIES
PUBLIC
   au3-preferences
   au3-registries
PRIVATE
   au3-fft
   au3-project-rate
   au3-strings
   au3-wave-track
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
