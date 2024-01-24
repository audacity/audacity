#[[
Synchronization objects and parallel algorithms
]]#

set( SOURCES
   MessageBuffer.h
   spinlock.h
)
set( LIBRARIES
)

audacity_header_only_library( lib-concurrency "${SOURCES}" "${LIBRARIES}"
   "" ""
)
