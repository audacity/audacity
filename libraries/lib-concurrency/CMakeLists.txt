#[[
Synchronization objects and parallel algorithms
]]#

set( SOURCES
   AtomicUniquePointer.h
   MessageBuffer.h
   NonInterfering.cpp
   NonInterfering.h
   ParallelProgress.h
   ParallelReduce.h
   Splittable.h
   spinlock.h
)
set( LIBRARIES
)

audacity_library( lib-concurrency "${SOURCES}" "${LIBRARIES}"
   "" ""
)
