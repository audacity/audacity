#[[
A small library defining various constants and utilities

No dependencies on wxWidgets.  Few symbols with linkage.  Many templates.

One very important utility is finally() as defined in The C++ Programming
Language, fourth edition, for describing ad-hoc RAII actions.

Historically, MemoryX.h began as a wrapper of C++11 <memory>, which by itself
did not yet provide std::make_unique.  That explains the name.

Audacity now uses C++17, and yet, need arose for other anticipations of future
standards.  It also provides other pervasively used utilities that don't
correspond to things in the standard.

]]#

set( SOURCES
   Foo.cpp
)
set( LIBRARIES
   lib-shuttlegui-interface
)
audacity_library( lib-foo "${SOURCES}" "${LIBRARIES}"
   "" ""
)
