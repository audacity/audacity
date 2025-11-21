#[[
Management of dynamically loaded libraries for add-on functionality, and for
effect, generator, and analyzer plug-ins.

Maintains persistent data in the configuration file for enablement of modules
and plug-ins, and preferred settings.
]]

# Include AU3 compatibility macros
include(${CMAKE_CURRENT_SOURCE_DIR}/../../au3defs.cmake)

set( SOURCES
   AsyncPluginValidator.cpp
   AsyncPluginValidator.h
   ConfigInterface.cpp
   ConfigInterface.h
   PluginIPCUtils.cpp
   PluginIPCUtils.h
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
   lib-xml-interface
   lib-ipc-interface
)
audacity_library( lib-module-manager "${SOURCES}" "${LIBRARIES}"
   "" ""
)
