#[[
Various abstract base classes related to modules and plugins.

ComponentInterfaceSymbol, which pairs an internal identifier string (suitable
for storing as a configuration file value) with a user-visible translatable
string.  It serves to "name" various things.

CommandParameters, for write and reading key-value pairs to and from strings.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   ComponentInterface.cpp
   ComponentInterface.h
   ComponentInterfaceSymbol.h
   EffectAutomationParameters.cpp
   EffectAutomationParameters.h
   EffectInterface.cpp
   EffectInterface.h
   PluginProvider.cpp
   PluginProvider.h
   SettingsVisitor.cpp
   SettingsVisitor.h
)
set( LIBRARIES
   au3-strings-interface
   au3-utility
)
audacity_library( au3-components "${SOURCES}" "${LIBRARIES}"
   "" ""
)
