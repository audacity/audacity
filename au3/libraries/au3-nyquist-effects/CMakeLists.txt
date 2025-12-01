#[[
Library of built-in effects. Implement the UI-agnostic APIs of au3-effects.
]]

set( TARGET au3-nyquist-effects )

set( SOURCES
   LoadNyquist.cpp
   LoadNyquist.h
   NyquistBase.cpp
   NyquistBase.h
)
set( LIBRARIES
   au3-effects-interface
   au3-label-track-interface
   au3-note-track-interface
   au3-time-track-interface
   au3-wave-track-settings
   libnyquist
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
