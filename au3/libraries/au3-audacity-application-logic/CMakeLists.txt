#[[
A UI-agnostic, high-level library implementing the behavior of Audacity.
]]

set( TARGET au3-audacity-application-logic )

set( SOURCES
   AudacityApplicationLogic.cpp
   AudacityApplicationLogic.h
   AudacityApplicationLogicTypes.h
)
set( LIBRARIES
PUBLIC
   au3-components
   au3-menus
   au3-strings
PRIVATE
   au3-audio-io
   au3-basic-ui
   au3-effects
   au3-module-manager
   au3-project
   au3-project-file-io
   au3-project-history
   au3-project-rate
   au3-time-frequency-selection
   au3-track-selection
   au3-viewport
   au3-wave-track
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
