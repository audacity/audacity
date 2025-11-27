#[[
Library of built-in effects. Implement the UI-agnostic APIs of au3-effects.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
)
audacity_library( au3-nyquist-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
