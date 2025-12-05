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
   au3-audio-io-interface
   au3-effects-interface
   au3-menus-interface
   au3-project-file-io-interface
   au3-viewport-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
