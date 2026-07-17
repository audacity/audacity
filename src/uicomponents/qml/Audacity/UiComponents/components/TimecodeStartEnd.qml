/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.UiComponents 1.0

Rectangle {
    id: root

    property alias startValue: startTimecode.value
    property alias endValue: endTimecode.value

    property real sampleRate: 0.0
    property real tempo: 0.0
    property int upperTimeSignature: 0
    property int lowerTimeSignature: 0

    property int currentFormat: 0

    property NavigationPanel navigationPanel: null
    property alias navigationColumnEnd: endTimecode.navigationColumnEnd
    property alias navigationRow: endTimecode.navigationRow

    property alias startAccessibleName: startTimecode.accessibleName
    property alias endAccessibleName: endTimecode.accessibleName

    signal startValueChangeRequested(var newValue)
    signal endValueChangeRequested(var newValue)
    signal formatChangeRequested(var newFormat)

    implicitWidth: layout.implicitWidth
    implicitHeight: layout.implicitHeight

    color: ui.theme.backgroundQuarternaryColor
    radius: 3
    border.color: ui.theme.strokeColor
    border.width: 1

    RowLayout {
        id: layout

        anchors.fill: parent
        spacing: 1

        Timecode {
            id: startTimecode

            appearance: Timecode.Appearance.Embedded

            mode: TimecodeModeSelector.TimePoint
            sampleRate: root.sampleRate
            tempo: root.tempo
            upperTimeSignature: root.upperTimeSignature
            lowerTimeSignature: root.lowerTimeSignature

            currentFormat: root.currentFormat

            showMenu: false

            navigation.panel: root.navigationPanel
            navigation.row: 1
            navigation.column: 1

            onValueChangeRequested: function (newValue) {
                root.startValueChangeRequested(newValue)
            }
        }

        Timecode {
            id: endTimecode

            appearance: Timecode.Appearance.Embedded

            mode: TimecodeModeSelector.TimePoint
            sampleRate: root.sampleRate
            tempo: root.tempo
            upperTimeSignature: root.upperTimeSignature
            lowerTimeSignature: root.lowerTimeSignature

            currentFormat: root.currentFormat

            navigation.panel: root.navigationPanel
            navigation.row: startTimecode.navigation.row
            navigation.column: startTimecode.navigationColumnEnd + 1

            onValueChangeRequested: function (newValue) {
                root.endValueChangeRequested(newValue)
            }

            onCurrentFormatChanged: function () {
                root.formatChangeRequested(currentFormat)
            }
        }
    }
}
