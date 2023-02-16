#[[
A simple RAII object TransactionScope.  Its dtor rolls back the project database
unless it is explicitly committed.

Also a facade that hides details of the implementation of the project database.

The main program can inject an implementation.  If this isn't done, then the
object has no effect.
]]

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
