/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Controls
import QtQuick.Layouts 1.15

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene
import Audacity.UiComponents

StyledDialogView {
    id: root

    property double start: 0
    property double end: 0

    contentWidth: 280
    contentHeight: 200

    SelectionStatusModel {
        id: selectionModel

        onCurrentFormatChanged: {
            timeIn.currentFormat = currentFormat
            timeOut.currentFormat = currentFormat
        }
    }

    PlayRegionController {
        id: playRegionController
    }

    Component.onCompleted: {
        selectionModel.init()
        playRegionController.beginPreview()
    }

    onStartChanged: {
        playRegionController.setPreviewStartTime(start)
    }

    onEndChanged: {
        playRegionController.setPreviewEndTime(end)
    }

    NavigationPanel {
        id: loopInNavPanel
        name: "LoopInInput"
        section: root.navigationSection
        order: 0
        direction: NavigationPanel.Horizontal
        enabled: timeIn.enabled && timeIn.visible
        accessible.name: loopInLabel.text
    }

    NavigationPanel {
        id: loopOutNavPanel
        name: "LoopOutInput"
        section: root.navigationSection
        order: 1
        direction: NavigationPanel.Horizontal
        enabled: timeOut.enabled && timeOut.visible
        accessible.name: loopOutLabel.text
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 2

        ColumnLayout {
            Layout.leftMargin: 16

            spacing: 8

            StyledTextLabel {
                id: loopInLabel

                Layout.fillWidth: true

                horizontalAlignment: Text.AlignLeft

                text: qsTrc("playback/loop", "Loop in")
            }

            Timecode {
                id: timeIn

                Layout.fillWidth: true
                Layout.fillHeight: false

                value: start
                mode: TimecodeModeSelector.Duration
                currentFormat: selectionModel.currentFormat
                sampleRate: selectionModel.sampleRate
                tempo: selectionModel.tempo
                upperTimeSignature: selectionModel.upperTimeSignature
                lowerTimeSignature: selectionModel.lowerTimeSignature

                navigation.panel: loopInNavPanel
                navigation.order: 0
                accessibleName: loopInLabel.text + " "

                onValueChanged: {
                    root.start = value
                }

                onCurrentFormatChanged: {
                    selectionModel.currentFormat = currentFormat
                }
            }
        }

        ColumnLayout {
            Layout.leftMargin: 16

            spacing: 8

            StyledTextLabel {
                id: loopOutLabel

                Layout.fillWidth: true

                horizontalAlignment: Text.AlignLeft

                text: qsTrc("playback/loop", "Loop out")
            }

            Timecode {
                id: timeOut

                Layout.fillWidth: true
                Layout.fillHeight: false

                value: end
                mode: TimecodeModeSelector.Duration
                currentFormat: selectionModel.currentFormat
                sampleRate: selectionModel.sampleRate
                tempo: selectionModel.tempo
                upperTimeSignature: selectionModel.upperTimeSignature
                lowerTimeSignature: selectionModel.lowerTimeSignature

                navigation.panel: loopOutNavPanel
                navigation.order: 0
                accessibleName: loopOutLabel.text + " "

                onValueChanged: {
                    root.end = value
                }

                onCurrentFormatChanged: {
                    selectionModel.currentFormat = currentFormat
                }
            }
        }

        SeparatorLine {
            Layout.topMargin: 16
        }

        ButtonBox {
            id: buttonBox

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 2

            Layout.fillWidth: true
            Layout.rightMargin: 8
            Layout.bottomMargin: 8

            FlatButton {
                id: cancelBtn
                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel
                minWidth: 80
                onClicked: {
                    playRegionController.endPreview()
                    root.reject()
                }
            }

            FlatButton {
                id: applyBtn
                text: qsTrc("global", "Apply")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.Apply
                minWidth: 80
                accentButton: true
                onClicked: {
                    root.ret = {
                        errcode: 0,
                        value: {
                            start: root.start,
                            end: root.end
                        }
                    }
                    playRegionController.endPreview()
                    root.hide()
                }
            }
        }
    }
}
