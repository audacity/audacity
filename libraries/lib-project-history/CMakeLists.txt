#[[
Management of undo and redo history of the project, stored as states, not
deltas.  There is not yet persistency of undo history across sessions.
]]

set( SOURCES
   ProjectHistory.cpp
   ProjectHistory.h
   UndoManager.cpp
   UndoManager.h
)
set( LIBRARIES
   lib-project-interface
   lib-transactions-interface
)
audacity_library( lib-project-history "${SOURCES}" "${LIBRARIES}"
   "" ""
)
