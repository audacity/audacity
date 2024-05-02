set(MODULE_NAME uicomponents)

set(MODULE_SOURCES
   IconCode.h
   MeterPanelHandler.cpp
   MeterPanelHandler.h
   ToolbarHandler.cpp
   ToolbarHandler.h
   ToolbarManager.cpp
   ToolbarManager.h
   TrackType.h
   VolumeControlHandler.cpp
   VolumeControlHandler.h
)

set(MODULE_QML_SOURCES
   qml/FlatButton.qml
   qml/FlatToggleButton.qml
   qml/LabelTrackControlPanel.qml
   qml/MeterPanel.qml
   qml/MonoTrackControlPanel.qml
   qml/PlayheadCursor.qml
   qml/PlayheadCursorHead.qml
   qml/StereoTrackControlPanel.qml
   qml/StyledDial.qml
   qml/StyledIconLabel.qml
   qml/StyledProgressBar.qml
   qml/StyledSlider.qml
   qml/StyledTextLabel.qml
   qml/TimeControl.qml
   qml/TrackVolumeMeter.qml
   qml/ToolbarGrip.qml
   qml/ToolbarSeparator.qml
   qml/VideoTrackControlPanel.qml
   qml/VolumeControl.qml
   qml/VolumeControlTester.qml
   qml/WaveClipBody.qml
   qml/WaveClipView.qml
)

audacity_qml_module( ${MODULE_NAME}
   URI "Audacity.UiComponents"
   STATIC
   VERSION 1.0
   DEPENDENCIES
      QtQuick
      Audacity.Ui
   QML_FILES
      ${MODULE_QML_SOURCES}
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      Qt6::Gui
      Qt6::Quick
)
