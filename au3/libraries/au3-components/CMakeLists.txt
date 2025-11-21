#[[
Various abstract base classes related to modules and plugins.

ComponentInterfaceSymbol, which pairs an internal identifier string (suitable
for storing as a configuration file value) with a user-visible translatable
string.  It serves to "name" various things.

CommandParameters, for write and reading key-value pairs to and from strings.
]]

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
   lib-strings-interface
   lib-utility
)
audacity_library( lib-components "${SOURCES}" "${LIBRARIES}"
   "" ""
)
