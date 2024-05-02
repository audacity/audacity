#[[
Startup time injections of Qt based implementations of services behind
toolkit-neutral facades
]]#

set(MODULE_NAME qtinitui)

set(QML_SOURCES
   GenericProgressDialog.qml
   MessageBox.qml
   MultiDialog.qml
   ProgressDialog.qml
   SimpleConfirmDialog.qml
)

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.QtInitUi"
   VERSION 1.0
   DEPENDENCIES
      QtQuick
      Audacity.Ui
      Audacity.UiComponents
   QML_FILES
      ${QML_SOURCES}
)
