#[[
Management of dynamically loaded libraries for add-on functionality, and for
effect, generator, and analyzer plug-ins.

Maintains persistent data in the configuration file for enablement of modules
and plug-ins, and preferred settings.
]]
set( SOURCES
   ConfigInterface.cpp
   ConfigInterface.h
   ModuleManager.cpp
   ModuleManager.h
   ModuleSettings.cpp
   ModuleSettings.h
   PluginInterface.cpp
   PluginInterface.h
   PluginManager.cpp
   PluginManager.h
)
set( LIBRARIES
   lib-files-interface
   PRIVATE
   wxBase
)
audacity_library( lib-module-manager "${SOURCES}" "${LIBRARIES}"
   "" ""
)
