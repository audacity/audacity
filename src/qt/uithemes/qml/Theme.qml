import QtQml

QtObject {
   objectName: "Theme"

   readonly property real itemOpacityDisabled: 0.3
   readonly property real buttonOpacityNormal: 0.7
   readonly property real buttonOpacityHit: 1.0
   readonly property real buttonOpacityHover: 0.5
   readonly property int borderWidth: 0
   readonly property int defaultButtonSize: 32

   readonly property real opacityLight: 0.25
   readonly property real opacityMedium: 0.5
   readonly property real opacityStrong: 0.75
   readonly property real opacityOpaque: 1.0

   property color backgroundColor1
   property color backgroundColor2
   property color backgroundColor3
   property color backgroundColor4
   property color fontColor1
   property color fontColor2
   property color buttonColor
   property color brandColor
   property color textFieldColor
   property color successColor
   property color dangerColor
   property color strokeColor1
   property color strokeColor2
   property color strokeColor3
   property color waveformRMSColor
   property color waveformHighlightColor
   property color waveformPeakColor
   property color clipStrokeColor
   property color clipHeaderColor
   property color textHighlightColor
   property color invalidInputColor
}
