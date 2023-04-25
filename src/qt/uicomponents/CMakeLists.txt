set(MODULE_NAME uicomponents)

qt_add_library(${MODULE_NAME} STATIC)

set_target_properties(${MODULE_NAME} PROPERTIES AUTOMOC ON)
target_link_libraries(${MODULE_NAME} PRIVATE Qt6::Quick)

set(MODULE_SOURCES
   ApplicationConfiguration.cpp
   ApplicationConfiguration.h
   VolumeControlHandler.cpp
   VolumeControlHandler.h
   MeterPanelHandler.cpp
   MeterPanelHandler.h
   IconCode.h
   TrackType.h
)

set(MODULE_QML_SOURCES
   qml/FlatButton.qml
   qml/LabelTrackControlPanel.qml
   qml/MeterPanel.qml
   qml/MonoTrackControlPanel.qml
   qml/PlayheadCursor.qml
   qml/PlayheadCursorHead.qml
   qml/StereoTrackControlPanel.qml
   qml/StyledDial.qml
   qml/StyledIconLabel.qml
   qml/StyledSlider.qml
   qml/StyledTextLabel.qml
   qml/TimeControl.qml
   qml/TrackVolumeMeter.qml
   qml/ToolbarGrip.qml
   qml/ToolbarSeparator.qml
   qml/VideoTrackControlPanel.qml
   qml/VolumeControl.qml
   qml/VolumeControlTester.qml
)

qt_add_qml_module(${MODULE_NAME}
   URI "Audacity.UiComponents"
   VERSION 1.0
   RESOURCE_PREFIX /${MODULE_NAME}
   DEPENDENCIES
      QtQuick
   QML_FILES
      ${MODULE_QML_SOURCES}
   SOURCES
      ${MODULE_SOURCES}
   NO_LINT
)

