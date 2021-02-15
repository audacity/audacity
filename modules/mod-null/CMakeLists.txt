set( TARGET mod-null )
set( DEFINES
   PRIVATE
      # This is needed until the transition to cmake is complete and
      # the Windows pragmas are removed from ModNullCallback.cpp.
      # Without it, the wxWidgets "debug.h" will define __WXDEBUG__
      # which then causes this module to emit library pragmas for the
      # debug versions of wxWidgets...even if the build is for Release.
      wxDEBUG_LEVEL=0
)
set( SOURCES
      ModNullCallback.cpp
      ModNullCallback.h
)
audacity_module( ${TARGET} "${SOURCES}" "Audacity"
   "${DEFINES}" "" )

set_target_properties( ${TARGET} PROPERTIES EXCLUDE_FROM_ALL YES )
