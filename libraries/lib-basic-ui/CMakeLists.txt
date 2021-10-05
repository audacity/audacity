#[[
This library defines a facade interface, BasicUI::Services, for a few basic
interactions with the user, such as showing simple dialog boxes, and for
enqueuing actions to the event loop, and yielding to to the event dispatcher.

The interface makes no mention of classes in wxWidgets.  Using this library
instead of making direct use of wxWidgets enlarges the parts of the program
that are toolkit neutral.

There is a global pointer to an instance of Services, and the main program is
expected, at startup, to create a static instance of a subclass of Services and
set the pointer.  If it does not, then calls to the non-member functions in
namespace BasicUI are no-ops.
]]#

set( SOURCES
   BasicUI.cpp
   BasicUI.h
   BasicUIPoint.h
)
set( LIBRARIES
   lib-strings-interface
   PRIVATE
   wxBase
)
audacity_library( lib-basic-ui "${SOURCES}" "${LIBRARIES}"
   "" ""
)
