#[[
Startup time injections of Qt based implementations of services behind
toolkit-neutral facades
]]#

set( SOURCES
   QMLEngineFactory.h
   ProjectQMLEnvironment.cpp
   ProjectQMLEnvironment.h
)

set( LIBRARIES
   lib-project-interface
   PUBLIC
      lib-registries-interface
      Qt6::Qml
)

audacity_library( lib-qt-init "${SOURCES}" "${LIBRARIES}" "" "")
