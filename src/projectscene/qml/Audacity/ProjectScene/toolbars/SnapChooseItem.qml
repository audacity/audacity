/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property alias text: labelItem.text
    property var contextMenuModel: null

    property bool selected: false

    property alias navigation: navCtrl

    signal handleMenuItem(var itemId)

    NavigationControl {
        id: navCtrl

        name: "SnapChooseItem"
        enabled: root.enabled && root.visible
        accessible.role: MUAccessible.ListItem
        accessible.name: root.text

        onActiveChanged: {
            if (!root.activeFocus) {
                root.forceActiveFocus()
            }
        }

        onTriggered: root.clicked()
    }

    Rectangle {
        id: backgroundItem
        anchors.fill: parent

        NavigationFocusBorder { navigationCtrl: navCtrl }

        color: ui.theme.textFieldColor
        border.color: ui.theme.strokeColor
        border.width: Math.max(ui.theme.borderWidth, 1)
        radius: 3
    }

    StyledTextLabel {
        id: labelItem

        anchors.left: parent.left
        anchors.leftMargin: 12
        anchors.right: dropIconItem.left
        anchors.verticalCenter: parent.verticalCenter

        horizontalAlignment: Text.AlignLeft
        wrapMode: Text.Wrap
        maximumLineCount: 1
    }

    MouseArea {
        id: mouseAreaItem
        anchors.fill: parent
        hoverEnabled: true

        onClicked: {
            menuLoader.toggleOpened(root.contextMenuModel)
        }

        onContainsMouseChanged: {
            if (!labelItem.truncated) {
                return
            }

            if (mouseArea.containsMouse) {
                ui.tooltip.show(root, labelItem.text)
            } else {
                ui.tooltip.hide(root)
            }
        }
    }

    StyledIconLabel {
        id: dropIconItem
        anchors.verticalCenter: parent.verticalCenter
        anchors.right: parent.right
        anchors.rightMargin: 8

        iconCode: IconCode.SMALL_ARROW_DOWN
    }

    states: [
        State {
            name: "HOVERED"
            when: mouseAreaItem.containsMouse && !mouseAreaItem.pressed
            PropertyChanges { target: backgroundItem; border.color: Utils.colorWithAlpha(ui.theme.accentColor, 0.6) }
        },

        State {
            name: "SELECTED"
            when: root.selected
            PropertyChanges { target: backgroundItem; border.color: ui.theme.accentColor }
        }
    ]

    StyledMenuLoader {
        id: menuLoader

        onHandleMenuItem: function(itemId) {
            root.handleMenuItem(itemId)
        }
    }
}
