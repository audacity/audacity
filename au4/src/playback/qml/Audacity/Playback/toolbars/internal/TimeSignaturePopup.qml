/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledPopupView {
    id: root

    property int upper: 0
    property int lower: 0

    signal upperChangeRequested(var newValue)
    signal lowerChangeRequested(var newValue)

    margins: 12
    
    contentWidth: 216
    contentHeight: 52
    
    ColumnLayout {
        id: content

        anchors.fill: parent

        spacing: 8

        StyledTextLabel {
            Layout.fillWidth: true

            text: qsTrc("playback", "Time signature")
            horizontalAlignment: Text.AlignLeft
        }

        Row {
            spacing: 12

            IncrementalPropertyControl {
                id: control

                implicitWidth: 80
                anchors.verticalCenter: parent.verticalCenter

                currentValue: root.upper
                step: 1
                decimals: 0
                maxValue: 128
                minValue: 1

                onValueEdited: function(newValue) {
                    root.upperChangeRequested(newValue)
                }
            }

            StyledTextLabel {
                width: 8
                anchors.verticalCenter: parent.verticalCenter
                font: ui.theme.largeBodyFont
                text: "/"
            }

            StyledDropdown {
                id: timeComboBox

                width: control.width
                anchors.verticalCenter: parent.verticalCenter

                currentIndex: timeComboBox.indexOfValue(root.lower)

                model: [
                    { "text": "2", "value": 2 },
                    { "text": "4", "value": 4 },
                    { "text": "8", "value": 8 },
                    { "text": "16", "value": 16 },
                    { "text": "32", "value": 32 },
                    { "text": "64", "value": 64 }
                ]

                onActivated: function(index, value) {
                    root.lowerChangeRequested(value)
                }
            }
        }
    }
}
