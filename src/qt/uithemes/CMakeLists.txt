set(MODULE_NAME uithemes)

qt_add_library(${MODULE_NAME} STATIC)

set_target_properties(${MODULE_NAME} PROPERTIES AUTOMOC ON)
target_link_libraries(${MODULE_NAME} PRIVATE Qt6::Quick)

set(MODULE_SOURCES
   UiThemeHandler.h
   UiThemeHandler.cpp
)

set(MODULES_QML_SINGLETON_SOURCES
   qml/Dark.qml
   qml/Light.qml
   qml/UiTheme.qml
)

set_source_files_properties(${MODULES_QML_SINGLETON_SOURCES} PROPERTIES QT_QML_SINGLETON_TYPE TRUE)

set(MODULE_QML_SOURCES
   qml/Theme.qml
)

qt_add_qml_module(${MODULE_NAME}
   URI "Audacity.UiThemes"
   VERSION 1.0
   RESOURCE_PREFIX /${MODULE_NAME}
   DEPENDENCIES
      QtQuick
   QML_FILES
      ${MODULE_QML_SOURCES}
      ${MODULES_QML_SINGLETON_SOURCES}
   SOURCES
      ${MODULE_SOURCES}
   NO_LINT
)

