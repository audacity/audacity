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
    au3-fft-interface
    au3-wave-track-interface
PRIVATE
    au3-strings-interface
    wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
