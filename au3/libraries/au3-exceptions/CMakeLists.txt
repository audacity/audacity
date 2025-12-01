#[[
Toolkit neutral library for exceptions.

Abstract class AudacityException with a member function for a delayed
handler action, enqueued in the main thread;

Some commonly useful subclasses, with delayed handlers that are no-ops or
are displays of messages to the user;

Function template GuardedCall which stops propagation of exceptions and
enqueues the delayed action.

But this library does NOT define a top-level handler for the whole application,
to catch all otherwise uncaught exceptions. That is a responsibility of high
level code.
]]

set( TARGET au3-exceptions )

set( SOURCES
   AudacityException.cpp
   AudacityException.h
   InconsistencyException.cpp
   InconsistencyException.h
   UserException.cpp
   UserException.h
)
set( LIBRARIES
   au3-utility-interface
   au3-basic-ui-interface
)

if( APPLE AND MIN_MACOS_VERSION VERSION_LESS "10.12")
   set( DEFINITIONS
      PUBLIC
         -DUNCAUGHT_EXCEPTIONS_UNAVAILABLE
   )
endif()

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "${DEFINITIONS}" ""
)
