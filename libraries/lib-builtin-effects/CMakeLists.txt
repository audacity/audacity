#[[
Library of built-in effects. Implement the UI-agnostic APIs of lib-effects.
]]

set( SOURCES
   AmplifyBase.cpp
   AmplifyBase.h
)
set( LIBRARIES
   lib-effects-interface
)
audacity_library( lib-builtin-effects "${SOURCES}" "${LIBRARIES}"
   "" ""
)
