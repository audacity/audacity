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

    required property string title

    property NavigationPanel navigationPanel: null
    property int navigationRow: 0

    signal runRequested
    signal renameRequested
    signal deleteRequested

    implicitHeight: prv.height

    color: hoverHandler.hovered ? ui.theme.extra["background_table_item_hover_color"] : ui.theme.extra["background_table_item_idle_color"]

    QtObject {
        id: prv

        readonly property int height: 44
        readonly property int sideMargin: 8
        readonly property int titleControlsSpacing: 16
        readonly property int buttonsSpacing: 6
        readonly property int buttonSize: 28
        readonly property int runButtonWidth: 56
        readonly property int runButtonMargins: 8
        readonly property int runButtonSpacing: 6
    }

    HoverHandler {
        id: hoverHandler
    }

    ContextMenuLoader {
        id: contextMenuLoader

        items: [
            {
                id: "edit",
                title: qsTrc("macromanager", "Edit"),
                enabled: false
            },
            {
                id: "rename",
                title: qsTrc("macromanager", "Rename")
            },
            {
                id: "run-on-files",
                title: qsTrc("macromanager", "Run on files…"),
                enabled: false
            },
            {
                id: "export",
                title: qsTrc("macromanager", "Export"),
                enabled: false
            },
            {},
            {
                id: "delete",
                title: qsTrc("macromanager", "Delete")
            }
        ]

        onHandleMenuItem: function (itemId) {
            switch (itemId) {
            case "rename":
                root.renameRequested()
                break
            case "delete":
                root.deleteRequested()
                break
            }
        }
    }

    SeparatorLine {
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: prv.sideMargin
        anchors.rightMargin: prv.sideMargin

        spacing: prv.titleControlsSpacing

        StyledTextLabel {
            Layout.fillWidth: true

            text: root.title
            horizontalAlignment: Qt.AlignLeft
            elide: Text.ElideRight
        }

        RowLayout {
            spacing: prv.buttonsSpacing

            FlatButton {
                id: runButton

                Layout.preferredWidth: prv.runButtonWidth
                Layout.preferredHeight: prv.buttonSize

                icon: IconCode.PLAY
                text: qsTrc("macromanager", "Run")
                orientation: Qt.Horizontal

                buttonType: FlatButton.Horizontal
                margins: prv.runButtonMargins

                navigation.panel: root.navigationPanel
                navigation.row: root.navigationRow
                navigation.column: 0
                navigation.name: "RunMacro"
                navigation.accessible.name: qsTrc("macromanager", "Run macro")

                contentItem: RowLayout {
                    spacing: prv.runButtonSpacing

                    StyledIconLabel {
                        Layout.alignment: Qt.AlignVCenter
                        iconCode: runButton.icon
                        font: runButton.iconFont
                        color: runButton.iconColor
                    }

                    StyledTextLabel {
                        Layout.alignment: Qt.AlignVCenter
                        text: runButton.text
                        font: runButton.textFont
                    }
                }

                onClicked: {
                    root.runRequested()
                }
            }

            FlatButton {
                id: menuButton

                Layout.preferredWidth: prv.buttonSize
                Layout.preferredHeight: prv.buttonSize

                icon: IconCode.MENU_THREE_DOTS

                navigation.panel: root.navigationPanel
                navigation.row: root.navigationRow
                navigation.column: 1
                navigation.name: "MacroContextMenu"
                navigation.accessible.name: qsTrc("macromanager", "Macro menu")

                onClicked: {
                    contextMenuLoader.show(menuButton.mapToItem(root, 0, menuButton.height))
                }
            }
        }
    }
}
