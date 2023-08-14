set(MODULE_NAME uithemes)

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

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.UiThemes"
   STATIC
   VERSION 1.0
   DEPENDENCIES
      QtQuick
   QML_FILES
      ${MODULE_QML_SOURCES}
      ${MODULES_QML_SINGLETON_SOURCES}
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      Qt6::Quick
)
