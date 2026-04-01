/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.BuiltinEffects
import Audacity.Nyquist

BuiltinEffectBase {
    id: root

    property string title: qsTrc("effects/nyquist", "Nyquist prompt")
    property bool isApplyAllowed: true
    numNavigationPanels: 1

    width: prv.dialogWidth
    implicitHeight: Math.min(prv.dialogHeight, prv.maxDialogHeight)

    builtinEffectModel: NyquistPromptViewModelFactory.createModel(root, root.instanceId)
    property alias viewModel: root.builtinEffectModel

    color: ui.theme.backgroundPrimaryColor

    property NavigationPanel navPanel: NavigationPanel {
        name: "NyquistPromptControls"
        enabled: root.enabled && root.visible
        direction: NavigationPanel.Vertical
        section: root.dialogView ? root.dialogView.navigationSection : null
        order: 1
    }

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

    ColumnLayout {
        anchors.fill: parent
        spacing: prv.spaceL

        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            StyledTextLabel {
                Layout.fillWidth: true
                text: qsTrc("effects", "Enter Nyquist command:")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Text.AlignLeft
            }

            FlatButton {
                text: qsTrc("effects", "Debug")
                transparent: true

                navigation.panel: root.navPanel
                navigation.order: 0

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

            navigation.panel: root.navPanel
            navigation.order: 1

            currentText: viewModel.commandText

            hint: qsTrc("effects", "Enter Nyquist code here…")

            onTextEditingFinished: function (newText) {
                viewModel.commandText = newText
            }
        }

        RowLayout {
            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter
            spacing: prv.spaceM

            FlatButton {
                text: qsTrc("effects", "Load")

                navigation.panel: root.navPanel
                navigation.order: 2

                onClicked: {
                    viewModel.loadScript()
                }
            }

            FlatButton {
                text: qsTrc("effects", "Save")
                accentButton: true

                navigation.panel: root.navPanel
                navigation.order: 3

                onClicked: {
                    viewModel.saveScript()
                }
            }
        }
    }
}
