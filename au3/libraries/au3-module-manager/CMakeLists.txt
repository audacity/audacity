#[[
Management of dynamically loaded libraries for add-on functionality, and for
effect, generator, and analyzer plug-ins.

Maintains persistent data in the configuration file for enablement of modules
and plug-ins, and preferred settings.
]]

set( TARGET au3-module-manager )

set( SOURCES
# those files aren't used in au4 atm
#   AsyncPluginValidator.cpp
#   AsyncPluginValidator.h
#   PluginIPCUtils.cpp
#   PluginIPCUtils.h
   ConfigInterface.cpp
   ConfigInterface.h
   ModuleManager.cpp
   ModuleManager.h
   ModuleSettings.cpp
   ModuleSettings.h
   PluginDescriptor.cpp
   PluginDescriptor.h
   PluginHost.cpp
   PluginHost.h
   PluginInterface.cpp
   PluginInterface.h
   PluginManager.cpp
   PluginManager.h
)
set( LIBRARIES
   au3-xml-interface
   au3-ipc-interface
   wxBase
)
audacity_library( ${TARGET} "${SOURCES}" "${LIBRARIES}"
   "" ""
)
