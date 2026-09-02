#[[
Vocabulary to describe toolbar menu items, their grouping, conditions to enable
them, and their actions.

A global MenuRegistry of menu descriptions and a visitor function.

A per-project CommandManager with a nested Populator type that visits the
registry, and can build menus, if a subclass overrides certain virtual functions
appropriately for the UI toolkit.
]]

set( TARGET au3-menus )

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
PUBLIC
   au3-preferences
   au3-project
   au3-registries
   au3-strings
   au3-utility
   au3-xml
PRIVATE
   au3-basic-ui
   au3-project-history
   wxBase
)

audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" "" )
