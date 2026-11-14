#[[
Inter-process pipe allowing control of Audacity by sending macro commands and
receiving responses
]]

set( SOURCES
   PipeServer.cpp
   ScripterCallback.cpp
)
set( DEFINES
   PRIVATE
      BUILDING_SCRIPT_PIPE

      # This is needed until the transition to cmake is complete and
      # the Windows pragmas are removed from ScripterCallback.cpp.
      # Without it, the wxWidgets "debug.h" will define __WXDEBUG__
      # which then causes this module to emit library pragmas for the
      # debug versions of wxWidgets...even if the build is for Release.
      wxDEBUG_LEVEL=0
)
set( LIBRARIES
   PRIVATE
      Audacity
)
audacity_module( mod-script-pipe "${SOURCES}" "${LIBRARIES}"
   "${DEFINES}" "" )
