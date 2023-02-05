#[[
Utilities for inter-thread communication
]]#

set( SOURCES
   Pipeline.cpp
   Pipeline.h
)
set( LIBRARIES
   lib-utility-interface
   wxBase
)
audacity_library( lib-concurrency "${SOURCES}" "${LIBRARIES}"
   "" ""
)
