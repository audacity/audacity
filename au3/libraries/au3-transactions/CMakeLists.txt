#[[
A simple RAII object TransactionScope.  Its dtor rolls back the project database
unless it is explicitly committed.

Also a facade that hides details of the implementation of the project database.

The main program can inject an implementation.  If this isn't done, then the
object has no effect.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   TransactionScope.cpp
   TransactionScope.h
)
set( LIBRARIES
   lib-exceptions-interface
)
audacity_library( lib-transactions "${SOURCES}" "${LIBRARIES}"
   ""
   ""
)
