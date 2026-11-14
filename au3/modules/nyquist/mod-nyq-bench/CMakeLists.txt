#[[
Experimental module for interactive development of Nyquist plugins
]]

set( TARGET mod-nyq-bench )
set( SOURCES
      NyqBench.cpp
      NyqBench.h
)
set( DEFINES
   PRIVATE
      # This is needed until the transition to cmake is complete and
      # the Windows pragmas are removed from NyqBench.cpp.  Without
      # it, the wxWidgets "debug.h" will define __WXDEBUG__ which
      # then causes this module to emit library pragmas for the debug
      # versions of wxWidgets...even if the build is for Release.
      wxDEBUG_LEVEL=0
)
set( LIBRARIES
   PRIVATE
      Audacity
)
audacity_module( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "${DEFINES}" "" )

set_target_properties( ${TARGET} PROPERTIES EXCLUDE_FROM_ALL YES )
