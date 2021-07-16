#[[
Various abstract base classes related to modules and plugins.

ComponentInterfaceSymbol, which pairs an internal identifier string (suitable
for storing as a configuration file value) with a user-visible translatable
string.  It serves to "name" various things.

CommandParameters, for write and reading key-value pairs to and from strings.
]]#

list( APPEND SOURCES
   ComponentInterface.cpp
   ComponentInterface.h
   ComponentInterfaceSymbol.h
   ConfigInterface.cpp
   ConfigInterface.h
   EffectAutomationParameters.cpp
   EffectAutomationParameters.h
   EffectInterface.cpp
   EffectInterface.h
   ModuleInterface.cpp
   ModuleInterface.h
   PluginInterface.cpp
   PluginInterface.h
)
audacity_library( lib-components "${SOURCES}"
   "lib-strings-interface;PRIVATE;wxBase"
   "" ""
)
