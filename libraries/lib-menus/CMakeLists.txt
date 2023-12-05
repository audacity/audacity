#[[
Vocabulary to describe toolbar menu items, their grouping, conditions to enable
them, and their actions.

A global MenuRegistry of menu descriptions and a visitor function.

A per-project CommandManager with a nested Populator type that visits the
registry, and can build menus, if a subclass overrides certain virtual functions
appropriately for the UI toolkit.
]]

set(SOURCES
   CommandContext.cpp
   CommandContext.h
   CommandFlag.cpp
   CommandFlag.h
   CommandFunctors.h
   CommandManager.cpp
   CommandManager.h
   CommandTargets.cpp
   CommandTargets.h
   Keyboard.cpp
   Keyboard.h
   MenuRegistry.cpp
   MenuRegistry.h
)

set( LIBRARIES
   lib-project-history-interface
)

audacity_library(lib-menus "${SOURCES}" "${LIBRARIES}"
   "" "" )
