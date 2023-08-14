import QtQml

QtObject {
   objectName: "Theme"

   readonly property real itemOpacityDisabled: 0.3
   readonly property real buttonOpacityNormal: 0.7
   readonly property real buttonOpacityHit: 1.0
   readonly property real buttonOpacityHover: 0.5
   readonly property int borderWidth: 0
   readonly property int defaultButtonSize: 32

   property color backgroundColor1
   property color backgroundColor2
   property color backgroundColor3
   property color fontColor1
   property color fontColor2
   property color buttonColor
   property color accentColor
   property color textFieldColor
   property color timecodeColor
   property color playColor
   property color recordColor
   property color strokeColor
   property color waveformRMSColor
   property color waveformHighlightColor
   property color waveformPeakColor
   property color clipStrokeColor
   property color clipHeaderColor
}
