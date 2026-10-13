/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledDialogView {
    id: root

    property string macroName: ""

    title: qsTrc("macromanager", "Delete macro")

    contentWidth: prv.contentWidth
    margins: prv.margins

    QtObject {
        id: prv

        readonly property int contentWidth: 320
        readonly property int margins: 16
        readonly property int iconSize: 48
        readonly property int iconTextsSpacing: 16
        readonly property int textsSpacing: 8
        readonly property int buttonsTopMargin: 16
        readonly property int buttonWidth: 64
        readonly property int buttonHeight: 28
    }

    onNavigationActivateRequested: {
        buttons.navigationPanel.requestActive()
    }

    ColumnLayout {
        width: parent.width

        spacing: 0

        RowLayout {
            Layout.fillWidth: true

            spacing: prv.iconTextsSpacing

            StyledIconLabel {
                Layout.alignment: Qt.AlignTop
                Layout.preferredWidth: prv.iconSize
                Layout.preferredHeight: prv.iconSize

                font.pixelSize: prv.iconSize
                iconCode: IconCode.WARNING
            }

            ColumnLayout {
                Layout.fillWidth: true

                spacing: prv.textsSpacing

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: qsTrc("macromanager", "Delete macro")
                    font: ui.theme.bodyBoldFont
                    horizontalAlignment: Qt.AlignLeft
                }

                StyledTextLabel {
                    Layout.fillWidth: true

                    text: qsTrc("macromanager", "\"%1\" will be deleted").arg(root.macroName)
                    horizontalAlignment: Qt.AlignLeft
                    wrapMode: Text.WordWrap
                }
            }
        }

        ButtonBox {
            id: buttons

            Layout.fillWidth: true
            Layout.topMargin: prv.buttonsTopMargin

            navigationPanel.section: root.navigationSection
            navigationPanel.order: 1

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("global", "Cancel")
                buttonRole: ButtonBoxModel.RejectRole
                buttonId: ButtonBoxModel.Cancel

                onClicked: {
                    root.reject()
                }
            }

            FlatButton {
                Layout.preferredWidth: prv.buttonWidth
                Layout.preferredHeight: prv.buttonHeight

                text: qsTrc("macromanager", "Confirm")
                buttonRole: ButtonBoxModel.AcceptRole
                buttonId: ButtonBoxModel.CustomButton + 1
                accentButton: true

                onClicked: {
                    root.ret = {
                        errcode: 0
                    }
                    root.hide()
                }
            }
        }
    }
}
