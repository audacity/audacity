#[[
A library defining various constants and utilities

No dependencies on wxWidgets.  Few symbols with linkage.  Many templates.

One very important utility is finally() as defined in The C++ Programming
Language, fourth edition, for describing ad-hoc RAII actions.

Historically, MemoryX.h began as a wrapper of C++11 <memory>, which by itself
did not yet provide std::make_unique.  That explains the name.

Audacity now uses C++17, and yet, need arose for other anticipations of future
standards.  It also provides other pervasively used utilities that don't
correspond to things in the standard.
]]

set( SOURCES
   AppEvents.cpp
   AppEvents.h
   BufferedStreamReader.cpp
   BufferedStreamReader.h
   CFResources.cpp
   CFResources.h
   CRTPBase.h
   Callable.cpp
   Callable.h
   CommandLineArgs.cpp
   CommandLineArgs.h
   Composite.cpp
   Composite.h
   GlobalVariable.h
   IteratorX.cpp
   IteratorX.h
   LockFreeQueue.h
   MathApprox.h
   MemoryX.cpp
   MemoryX.h
   MessageBuffer.h
   ModuleConstants.cpp
   ModuleConstants.h
   MemoryStream.cpp
   MemoryStream.h
   Observer.cpp
   Observer.h
   PackedArray.h
   spinlock.h
   Tuple.cpp
   Tuple.h
   TypeEnumerator.cpp
   TypeEnumerator.h
   TypeList.cpp
   TypeList.h
   TypeListVisitor.h
   TypeSwitch.cpp
   TypeSwitch.h
   TypedAny.h
   Variant.cpp
   Variant.h
)
set( LIBRARIES
)

if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
    find_library(CORE_FOUNDATION CoreFoundation)
    set( LIBRARIES PRIVATE ${CORE_FOUNDATION})
endif()

audacity_library( lib-utility "${SOURCES}" "${LIBRARIES}"
   "" ""
)
