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
PUBLIC
   au3-components
   au3-effects
   au3-files
   au3-math
   au3-module-manager
   au3-utility
   au3-wave-track
PRIVATE
   au3-basic-ui
   au3-command-parameters
   au3-label-track
   #au3-note-track # don't link for now as we need USE_MIDI
   au3-preferences
   au3-project
   au3-project-rate
   au3-strings
   au3-time-frequency-selection
   au3-time-track
   au3-track
   au3-wave-track-settings
   libnyquist
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
