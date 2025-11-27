#[[
A UI-agnostic, high-level library implementing the behavior of Audacity.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

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
)
audacity_library( au3-audacity-application-logic "${SOURCES}" "${LIBRARIES}"
   "" ""
)
