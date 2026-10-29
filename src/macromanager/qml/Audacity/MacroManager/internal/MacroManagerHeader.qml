/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    property NavigationPanel navigationPanel: null
    property int navigationOrderStart: 0

    property alias menuModel: menuButton.menuModel

    signal newMacroRequested
    signal handleMenuItemRequested(string itemId)

    implicitHeight: prv.height

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        readonly property int height: 44
        readonly property int sideMargin: 8
        readonly property int buttonsSpacing: 4
        readonly property int buttonVerticalMargin: 8
        readonly property int newMacroButtonMargins: 8
        readonly property int newMacroButtonSpacing: 6
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: prv.sideMargin
        anchors.rightMargin: prv.sideMargin

        spacing: 0

        StyledTextLabel {
            Layout.fillWidth: true

            text: qsTrc("macromanager", "Macros")
            font: ui.theme.bodyBoldFont
            horizontalAlignment: Qt.AlignLeft
        }

        RowLayout {
            Layout.fillHeight: true
            Layout.topMargin: prv.buttonVerticalMargin
            Layout.bottomMargin: prv.buttonVerticalMargin

            spacing: prv.buttonsSpacing

            FlatButton {
                id: newMacroButton

                Layout.fillHeight: true

                icon: IconCode.PLUS
                text: qsTrc("macromanager", "New macro")
                orientation: Qt.Horizontal
                accentButton: true

                buttonType: FlatButton.Horizontal
                margins: prv.newMacroButtonMargins

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart
                navigation.name: "NewMacro"

                contentItem: RowLayout {
                    spacing: prv.newMacroButtonSpacing

                    StyledIconLabel {
                        Layout.alignment: Qt.AlignVCenter
                        iconCode: newMacroButton.icon
                        font: newMacroButton.iconFont
                        color: newMacroButton.iconColor
                    }

                    StyledTextLabel {
                        Layout.alignment: Qt.AlignVCenter
                        text: newMacroButton.text
                        font: newMacroButton.textFont
                    }
                }

                onClicked: {
                    root.newMacroRequested()
                }
            }

            MenuButton {
                id: menuButton

                Layout.preferredWidth: newMacroButton.height
                Layout.preferredHeight: newMacroButton.height

                transparent: false

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrderStart + 1
                navigation.name: "MacroManagerMenu"
                navigation.accessible.name: qsTrc("macromanager", "Macro manager menu")

                onHandleMenuItem: function (itemId) {
                    root.handleMenuItemRequested(itemId)
                }
            }
        }
    }
}
