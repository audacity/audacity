/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

RowLayout {
    id: root

    property string title: ""
    property string current: ""
    property var model: null

    property bool isOptionEnabled: false
    property bool allowOptionToggle: true

    property var navigation: allowOptionToggle ? optionCheckBox.navigation : navCtrl // todo

    spacing: 6

    signal isOptionEnableChangeRequested(var enable)
    signal handleMenuItem(var itemId)

    StyledTextLabel {
        Layout.alignment: Qt.AlignVCenter

        text: root.title
    }

    CheckBox {
        id: optionCheckBox

        Layout.alignment: Qt.AlignVCenter

        checked: root.isOptionEnabled
        visible: root.allowOptionToggle

        onClicked: function() {
            root.isOptionEnableChangeRequested(!optionCheckBox.checked)
        }
    }

    Item {
        id: dropdown

        Layout.fillWidth: true
        Layout.fillHeight: true

        enabled: root.allowOptionToggle ? root.isOptionEnabled : true

        function openMenu() {
            menuLoader.toggleOpened(root.model)
        }

        NavigationControl {
            id: navCtrl

            name: "DropdownWithTitleItem"
            enabled: dropdown.enabled && dropdown.visible
            accessible.role: MUAccessible.ListItem
            accessible.name: labelItem.text

            onActiveChanged: {
                if (!dropdown.activeFocus) {
                    dropdown.forceActiveFocus()
                }
            }

            onTriggered: {
                dropdown.openMenu()
            }
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

            text: root.current
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.Wrap
            maximumLineCount: 1
        }

        MouseArea {
            id: mouseAreaItem
            anchors.fill: parent
            hoverEnabled: dropdown.enabled

            onClicked: {
                dropdown.openMenu()
            }

            onPressed: {
                ui.tooltip.hide(dropdown, true)
            }

            onContainsMouseChanged: {
                if (!labelItem.truncated || menuLoader.isMenuOpened) {
                    return
                }

                if (mouseAreaItem.containsMouse) {
                    ui.tooltip.show(dropdown, labelItem.text)
                } else {
                    ui.tooltip.hide(dropdown)
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
                name: "OPENED"
                when: menuLoader.isMenuOpened
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
}
