/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Video

StyledDialogView {
    id: root

    title: qsTrc("video", "Video offset")

    contentWidth: 320
    contentHeight: 176

    margins: 16

    VideoPanelModel {
        id: model
    }

    //! Edited locally and only written on Apply, so a half-typed number does
    //! not send the decoder chasing frames on every keystroke.
    property double pendingOffset: 0

    Component.onCompleted: {
        model.init()
        root.pendingOffset = model.offset
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 12

        StyledTextLabel {
            Layout.fillWidth: true
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
            text: qsTrc("video", "Shift the picture along the timeline. A positive offset moves the video later, a negative one earlier.")
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            IncrementalPropertyControl {
                id: offsetControl

                Layout.fillWidth: true

                currentValue: root.pendingOffset

                // An hour either way is far more than any real misalignment,
                // and the arrows move 10 ms a click; typing gives the full
                // millisecond resolution the three decimals allow.
                minValue: -3600
                maxValue: 3600
                decimals: 3
                step: 0.01
                measureUnitsSymbol: qsTrc("video", "s")

                navigation.panel: navPanel
                navigation.order: 1

                onValueEdited: function (newValue) {
                    root.pendingOffset = newValue
                }
            }

            FlatButton {
                text: qsTrc("video", "Reset")
                enabled: root.pendingOffset !== 0
                navigation.panel: navPanel
                navigation.order: 2
                onClicked: root.pendingOffset = 0
            }
        }

        Item {
            Layout.fillHeight: true
        }

        RowLayout {
            Layout.fillWidth: true
            spacing: 8

            Item {
                Layout.fillWidth: true
            }

            FlatButton {
                text: qsTrc("global", "Cancel")
                navigation.panel: navPanel
                navigation.order: 3
                onClicked: root.reject()
            }

            FlatButton {
                text: qsTrc("global", "Apply")
                accentButton: true
                navigation.panel: navPanel
                navigation.order: 4
                onClicked: {
                    model.offset = root.pendingOffset
                    root.accept()
                }
            }
        }
    }

    NavigationPanel {
        id: navPanel
        name: "VideoOffsetDialog"
        section: root.navigationSection
        direction: NavigationPanel.Horizontal
        order: 1
    }
}
