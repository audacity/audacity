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

    required property int step
    required property string title

    property int sideMargin: 0
    property int handleWidth: 0
    property int stepWidth: 0
    property int actionsWidth: 0

    property NavigationPanel navigationPanel: null
    property int navigationRow: 0

    signal editRequested
    signal deleteRequested

    implicitHeight: prv.height

    color: hoverHandler.hovered ? ui.theme.extra["background_table_item_hover_color"] : ui.theme.extra["background_table_item_idle_color"]

    QtObject {
        id: prv

        readonly property int height: 44
        readonly property int buttonSize: 28
        readonly property int buttonsSpacing: 4
    }

    HoverHandler {
        id: hoverHandler
    }

    SeparatorLine {
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
    }

    ContextMenuLoader {
        id: contextMenuLoader

        items: [
            {
                id: "move-up",
                title: qsTrc("macromanager", "Move up"),
                enabled: false
            },
            {
                id: "move-down",
                title: qsTrc("macromanager", "Move down"),
                enabled: false
            },
            {},
            {
                id: "delete",
                title: qsTrc("macromanager", "Delete")
            }
        ]

        onHandleMenuItem: function (itemId) {
            if (itemId === "delete") {
                root.deleteRequested()
            }
        }
    }

    RowLayout {
        anchors.fill: parent
        anchors.rightMargin: root.sideMargin

        spacing: 0

        StyledIconLabel {
            Layout.preferredWidth: root.handleWidth
            Layout.fillHeight: true

            iconCode: IconCode.TOOLBAR_GRIP
        }

        SeparatorLine {}

        StyledTextLabel {
            Layout.preferredWidth: root.stepWidth
            Layout.fillHeight: true

            text: root.step
            horizontalAlignment: Qt.AlignLeft
            leftPadding: root.sideMargin
        }

        SeparatorLine {}

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.fillHeight: true

            text: root.title
            horizontalAlignment: Qt.AlignLeft
            elide: Text.ElideRight
            leftPadding: root.sideMargin
        }

        SeparatorLine {}

        RowLayout {
            Layout.preferredWidth: root.actionsWidth

            spacing: prv.buttonsSpacing

            FlatButton {
                Layout.preferredWidth: prv.buttonSize
                Layout.preferredHeight: prv.buttonSize
                Layout.leftMargin: root.sideMargin

                icon: IconCode.EDIT
                transparent: true

                navigation.panel: root.navigationPanel
                navigation.row: root.navigationRow
                navigation.column: 0
                navigation.name: "EditCommand"
                navigation.accessible.name: qsTrc("macromanager", "Edit command")

                onClicked: {
                    root.editRequested()
                }
            }

            FlatButton {
                id: menuButton

                Layout.preferredWidth: prv.buttonSize
                Layout.preferredHeight: prv.buttonSize

                icon: IconCode.MENU_THREE_DOTS
                transparent: true

                navigation.panel: root.navigationPanel
                navigation.row: root.navigationRow
                navigation.column: 1
                navigation.name: "CommandContextMenu"
                navigation.accessible.name: qsTrc("macromanager", "Command menu")

                onClicked: {
                    contextMenuLoader.show(menuButton.mapToItem(root, 0, menuButton.height))
                }
            }
        }
    }
}
