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
    required property string code
    required property string title
    required property bool hasParams

    property int sideMargin: 0
    property int handleWidth: 0
    property int stepWidth: 0
    property int actionsWidth: 0

    property string dragMimeType: ""
    property int dragRow: -1

    property NavigationPanel navigationPanel: null
    property int navigationRow: 0
    property bool innerNavigationActive: false

    property NavigationPanel innerNavigationPanel: NavigationPanel {
        name: "CommandActions"
        enabled: root.enabled && root.visible && root.innerNavigationActive
        section: root.navigationPanel ? root.navigationPanel.section : null
        direction: NavigationPanel.Horizontal
        order: root.navigationPanel ? root.navigationPanel.order : 0
        accessible.name: root.title

        onActiveChanged: {
            if (root.innerNavigationPanel.active) {
                return
            }

            Qt.callLater(function () {
                if (!root.innerNavigationPanel.active) {
                    root.innerNavigationActive = false
                }
            })
        }

        onNavigationEvent: function (event) {
            if (event.type === NavigationEvent.Escape) {
                root.innerNavigationActive = false
                actionsNavCtrl.requestActive()
                event.accepted = true
            }
        }
    }

    property bool canMoveUp: false
    property bool canMoveDown: false

    signal editRequested
    signal deleteRequested
    signal moveUpRequested
    signal moveDownRequested

    implicitHeight: prv.height

    color: hoverHandler.hovered ? ui.theme.extra["background_table_item_hover_color"] : ui.theme.extra["background_table_item_idle_color"]
    opacity: root.Drag.active ? ui.theme.itemOpacityDisabled : 1.0

    Drag.dragType: Drag.Automatic
    Drag.supportedActions: Qt.MoveAction
    Drag.active: handleArea.drag.active
    Drag.mimeData: {
        const data = {}
        data[root.dragMimeType] = JSON.stringify({
            row: root.dragRow,
            code: root.code,
            title: root.title
        })
        return data
    }

    QtObject {
        id: prv

        readonly property int height: 44
        readonly property int buttonSize: 28
        readonly property int buttonsSpacing: 4
    }

    HoverHandler {
        id: hoverHandler
    }

    Item {
        id: dragTarget
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
                enabled: root.canMoveUp
            },
            {
                id: "move-down",
                title: qsTrc("macromanager", "Move down"),
                enabled: root.canMoveDown
            },
            {},
            {
                id: "delete",
                title: qsTrc("macromanager", "Delete")
            }
        ]

        onHandleMenuItem: function (itemId) {
            switch (itemId) {
            case "move-up":
                root.moveUpRequested()
                break
            case "move-down":
                root.moveDownRequested()
                break
            case "delete":
                root.deleteRequested()
                break
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

            MouseArea {
                id: handleArea

                anchors.fill: parent

                cursorShape: Qt.SizeAllCursor

                drag.target: dragTarget

                onPressed: {
                    root.grabToImage(function (result) {
                        root.Drag.imageSource = result.url
                    })
                }
            }
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

        Item {
            Layout.preferredWidth: root.actionsWidth
            Layout.fillHeight: true

            NavigationControl {
                id: actionsNavCtrl

                name: "CommandRow"
                panel: root.navigationPanel
                row: root.navigationRow
                column: 0
                enabled: root.enabled && root.visible

                accessible.role: MUAccessible.ListItem
                accessible.name: root.title

                onTriggered: {
                    root.innerNavigationActive = true
                    const firstButton = editButton.enabled ? editButton : menuButton
                    firstButton.navigation.requestActive()
                }

                onActiveChanged: {
                    if (actionsNavCtrl.active) {
                        root.innerNavigationActive = false
                    }
                }
            }

            NavigationFocusBorder {
                navigationCtrl: actionsNavCtrl
                drawOutsideParent: false
            }

            RowLayout {
                anchors.fill: parent

                spacing: prv.buttonsSpacing

                FlatButton {
                    id: editButton

                    Layout.preferredWidth: prv.buttonSize
                    Layout.preferredHeight: prv.buttonSize
                    Layout.leftMargin: root.sideMargin

                    icon: IconCode.EDIT
                    transparent: true
                    enabled: root.hasParams

                    navigation.panel: root.innerNavigationPanel
                    navigation.order: 0
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

                    navigation.panel: root.innerNavigationPanel
                    navigation.order: 1
                    navigation.name: "CommandContextMenu"
                    navigation.accessible.name: qsTrc("macromanager", "Command menu")

                    onClicked: {
                        contextMenuLoader.show(menuButton.mapToItem(root, 0, menuButton.height))
                    }
                }
            }
        }
    }
}
