#[[
A UI-agnostic, high-level library implementing the behavior of Audacity.
]]

set( SOURCES
   EffectManager.cpp
   EffectManager.h
   AudacityApplicationLogic.cpp
   AudacityApplicationLogic.h
   AudacityApplicationLogicTypes.h
)
set( LIBRARIES
   lib-audio-io-interface
   lib-effects-interface
   lib-menus-interface
   lib-project-file-io-interface
   lib-viewport-interface
)
audacity_library( lib-audacity-application-logic "${SOURCES}" "${LIBRARIES}"
   "" ""
)
