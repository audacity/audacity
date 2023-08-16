#[[
Startup time injections of Qt based implementations of services behind
toolkit-neutral facades
]]#

add_subdirectory(qtinitui)

set(SOURCES
   QMLEngineFactory.h
   ProjectQMLEnvironment.cpp
   ProjectQMLEnvironment.h
   QtQuickUiServices.cpp
   QtQuickUiServices.h
   QtQuickWindowPlacement.cpp
   QtQuickWindowPlacement.h
)

set(LIBRARIES
   lib-project-interface
   Qt6::Qml
   Qt6::Widgets
   Qt6::Quick   
   lib-string-utils-interface
   PUBLIC
      lib-registries-interface
      lib-basic-ui-interface
      lib-registries-interface
   INTERFACE
      qtinituiplugin
)

set(DEFINES
   PRIVATE
      -DQT_INIT_QML_IMPORT_PATH=":${AUDACITY_QML_RESOURCE_PREFIX}"
)

audacity_library( lib-qt-init "${SOURCES}" "${LIBRARIES}" "${DEFINES}" "")
