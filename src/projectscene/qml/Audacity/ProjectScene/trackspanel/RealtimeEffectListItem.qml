import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects

ListItemBlank {
    id: root

    property var item: null
    property var availableEffects: null
    property var handleMenuItemWithState: null

    height: 24
    clip: false // should be true?
    background.color: "transparent"
    hoverHitColor: "transparent"

    RowLayout {
        anchors.fill: parent
        spacing: 4

        FlatButton {
            id: gripButton

            Layout.preferredWidth: 14
            Layout.preferredHeight: 16
            Layout.alignment: Qt.AlignVCenter
            backgroundRadius: 0

            visible: true
            transparent: true
            hoverHitColor: ui.theme.buttonColor
            mouseArea.cursorShape: Qt.SizeAllCursor

            // do not accept buttons as FlatButton's mouseArea will override
            // DockTitleBar mouseArea and effect will not be draggable
            mouseArea.acceptedButtons: Qt.NoButton

            contentItem: StyledIconLabel {
                iconCode: IconCode.TOOLBAR_GRIP
            }

        }

        BypassEffectButton {
            Layout.margins: 0
            Layout.alignment: Qt.AlignVCenter
            Layout.preferredWidth: root.height
            Layout.minimumHeight: root.height
            Layout.maximumHeight: root.height

            isMasterEffect: item && item.isMasterEffect
            accentButton: item && item.isActive

            onClicked: {
                item.isActive = item && !item.isActive
            }
        }

        // Wrappinng a FlatButton in a Rectangle because of two bad reasons:
        // * I don't find a `border` property for `FlatButton`
        // * Somehow, the button's color is a bit darkened it direct child of the RowLayout
        Rectangle {
            id: effectNameRect

            border.color: ui.theme.strokeColor
            border.width: 1
            radius: effectNameButton.backgroundRadius
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.preferredWidth: 148

            FlatButton {
                id: effectNameButton

                anchors.fill: parent
                normalColor: ui.theme.backgroundPrimaryColor

                StyledTextLabel {
                    anchors.fill: parent
                    anchors.leftMargin: 6
                    anchors.rightMargin: 6
                    id: trackNameLabel
                    horizontalAlignment: Text.AlignLeft
                    verticalAlignment: Text.AlignVCenter
                    text: root.item ? root.item.effectName() : ""
                }

                onClicked: {
                    root.item.toggleDialog()
                }
            }
        }

        // Wrapping a FlatButton for the same reasons as above.
        Rectangle {
            id: chooseEffectRect

            border.color: ui.theme.strokeColor
            border.width: 1
            radius: effectNameButton.backgroundRadius
            Layout.fillHeight: true
            Layout.preferredWidth: height

            FlatButton {
                id: chooseEffectDropdown

                anchors.fill: parent
                normalColor: ui.theme.backgroundPrimaryColor
                icon: IconCode.SMALL_ARROW_DOWN

                onClicked: {
                    effectMenuLoader.toggleOpened(root.availableEffects)
                }

                StyledMenuLoader {
                    id: effectMenuLoader

                    onHandleMenuItem: function(menuItem) {
                        root.handleMenuItemWithState(menuItem, root.item)
                    }
                }
            }
        }
    }
}
