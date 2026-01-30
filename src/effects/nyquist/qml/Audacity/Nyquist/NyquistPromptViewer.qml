/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

Rectangle {
    id: root

    required property int instanceId

    implicitWidth: prv.dialogWidth
    implicitHeight: Math.min(prv.dialogHeight, prv.maxDialogHeight)

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        readonly property int spaceS: 4
        readonly property int spaceM: 8
        readonly property int spaceL: 12
        readonly property int spaceXL: 16
        readonly property int spaceXXL: 24

        readonly property int borderWidth: 1
        readonly property int borderRadius: 4

        readonly property int dialogWidth: 640
        readonly property int dialogHeight: 480
        readonly property int maxDialogHeight: 640
    }

    property var viewModel: NyquistPromptViewModelFactory.createModel(root, root.instanceId)

    Component.onCompleted: {
        viewModel.init()
    }

    ColumnLayout {
        anchors.fill: parent
        anchors.margins: prv.spaceXL
        spacing: prv.spaceL

        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            StyledTextLabel {
                Layout.fillWidth: true
                text: qsTrc("effects", "Enter Nyquist Command:")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Text.AlignLeft
            }

            FlatButton {
                text: qsTrc("effects", "Debug")
                transparent: true
                onClicked: {
                    viewModel.debugEffect()
                }
            }
        }

        // Multi-line text input area
        TextInputArea {
            id: commandTextArea
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.minimumHeight: 200

            currentText: viewModel.commandText

            hint: qsTrc("effects", "Enter Nyquist code here...")

            onTextChanged: function (newText) {
                viewModel.commandText = newText
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            spacing: prv.spaceM

            FlatButton {
                text: qsTrc("effects", "Load")
                onClicked: {
                    viewModel.loadScript()
                }
            }

            FlatButton {
                text: qsTrc("effects", "Save")
                accentButton: true
                onClicked: {
                    viewModel.saveScript()
                }
            }
        }
    }

    // Preview methods - delegate to viewModel
    function startPreview() {
        viewModel.startPreview()
    }

    function stopPreview() {
        viewModel.stopPreview()
    }

    property bool isApplyAllowed: true
    property bool usesPresets: false  // Nyquist Prompt doesn't use presets
    property bool isPreviewing: viewModel.isPreviewing
}
