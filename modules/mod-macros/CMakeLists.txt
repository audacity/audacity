#[[
Adds the user interface for management of macros (formerly called "chains"),
management of the macros directory, saving and loading and interpretation of
macro files, and menu items.

The dispatch of individual commands is at a lower level outside of this module.
]]

set( SOURCES
   BatchCommandDialog.cpp
   BatchCommandDialog.h
   BatchCommands.cpp
   BatchCommands.h
   BatchEvalCommand.cpp
   BatchEvalCommand.h
   BatchPrefs.cpp
   BatchPrefs.h
   BatchProcessDialog.cpp
   BatchProcessDialog.h
)
set( LIBRARIES
   Audacity
)
audacity_module( mod-macros "${SOURCES}" "${LIBRARIES}"
   "" "" )
