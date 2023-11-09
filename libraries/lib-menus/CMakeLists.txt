#[[
Vocabulary to describe toolbar menu items, their grouping, conditions to enable
them, and their actions.

A global registry of menu descriptions and a visitor function.

It does not contain a visitor to build menus, becuase that is not
toolkit-neutral.
]]

set(SOURCES
   CommandContext.cpp
   CommandContext.h
   CommandFlag.cpp
   CommandFlag.h
   CommandFunctors.h
   CommandTargets.cpp
   CommandTargets.h
   Keyboard.cpp
   Keyboard.h
   MenuRegistry.cpp
   MenuRegistry.h
)

set( LIBRARIES
   lib-project-interface
)

audacity_library(lib-menus "${SOURCES}" "${LIBRARIES}"
   "" "" )
