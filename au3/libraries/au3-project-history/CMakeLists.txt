#[[
Management of undo and redo history of the project, stored as states, not
deltas.  There is not yet persistency of undo history across sessions.
]]

set( TARGET au3-project-history )

set( SOURCES
   ProjectHistory.cpp
   ProjectHistory.h
   UndoManager.cpp
   UndoManager.h
)
set( LIBRARIES
PUBLIC
   au3-registries
   au3-utility
PRIVATE
   au3-basic-ui
   au3-project
   au3-strings
   au3-transactions
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
