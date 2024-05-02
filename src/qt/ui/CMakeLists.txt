set(MODULE_NAME ui)

set(MODULE_SOURCES
   Theme.h
   Theme.cpp
   Ui.h
   Ui.cpp
)

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.Ui"
   STATIC
   VERSION 1.0
   DEPENDENCIES
      QtQuick
      Audacity.Ui
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      Qt6::Quick
      uiplugin
      lib-qt-init
      lib-registries
)
