set(MODULE_NAME uithemes)

set(MODULE_SOURCES
   UiTheme.h
   UiTheme.cpp
)

set(MODULE_QML_SOURCES
   qml/Theme.qml
   qml/Light.qml
   qml/Dark.qml
)

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.UiThemes"
   STATIC
   VERSION 1.0
   DEPENDENCIES
      QtQuick
   QML_FILES
      ${MODULE_QML_SOURCES}
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      Qt6::Quick
)
