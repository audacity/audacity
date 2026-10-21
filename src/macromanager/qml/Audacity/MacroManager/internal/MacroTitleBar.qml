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

    property string title: ""
    property bool isBuiltin: false

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "MacroTitleBar"
        direction: NavigationPanel.Horizontal
        enabled: root.enabled && root.visible
        accessible.name: root.title
    }

    signal renameRequested
    signal exportRequested
    signal removeAllStepsRequested
    signal deleteRequested

    implicitHeight: prv.height

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv

        readonly property int height: 44
        readonly property int sideMargin: 8
        readonly property int titleButtonSpacing: 8
        readonly property int buttonSize: 28
    }

    ContextMenuLoader {
        id: contextMenuLoader

        items: [
            {
                id: "rename",
                title: qsTrc("macromanager", "Rename macro"),
                enabled: !root.isBuiltin
            },
            {
                id: "export",
                title: qsTrc("macromanager", "Export macro…")
            },
            {},
            {
                id: "remove-all-steps",
                title: qsTrc("macromanager", "Remove all steps")
            },
            {
                id: "delete",
                title: qsTrc("macromanager", "Delete macro"),
                enabled: !root.isBuiltin
            }
        ]

        onHandleMenuItem: function (itemId) {
            switch (itemId) {
            case "rename":
                root.renameRequested()
                break
            case "export":
                root.exportRequested()
                break
            case "remove-all-steps":
                root.removeAllStepsRequested()
                break
            case "delete":
                root.deleteRequested()
                break
            }
        }
    }

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: prv.sideMargin
        anchors.rightMargin: prv.sideMargin

        spacing: prv.titleButtonSpacing

        StyledTextLabel {
            Layout.fillWidth: true

            text: root.title
            font: ui.theme.bodyBoldFont
            horizontalAlignment: Qt.AlignLeft
            elide: Text.ElideRight
        }

        FlatButton {
            id: menuButton

            Layout.preferredWidth: prv.buttonSize
            Layout.preferredHeight: prv.buttonSize

            icon: IconCode.MENU_THREE_DOTS
            normalColor: root.color

            navigation.panel: root.navigationPanel
            navigation.order: 0
            navigation.name: "MacroMenu"
            navigation.accessible.name: qsTrc("macromanager", "Macro menu")

            onClicked: {
                contextMenuLoader.show(menuButton.mapToItem(root, 0, menuButton.height))
            }
        }
    }
}
