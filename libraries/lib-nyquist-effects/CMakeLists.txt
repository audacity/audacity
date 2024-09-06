#[[
Library of built-in effects. Implement the UI-agnostic APIs of lib-effects.
]]

set( SOURCES
   LoadNyquist.cpp
   LoadNyquist.h
   NyquistBase.cpp
   NyquistBase.h
)
set( LIBRARIES
   lib-effects-interface
   lib-label-track-interface
   lib-note-track-interface
   lib-time-track-interface
   libnyquist
)
audacity_library( lib-nyquist-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
