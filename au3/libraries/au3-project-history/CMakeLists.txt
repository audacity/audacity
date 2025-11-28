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
   au3-project-interface
   au3-transactions-interface
)
audacity_library( au3-project-history "${SOURCES}" "${LIBRARIES}"
   "" ""
)
