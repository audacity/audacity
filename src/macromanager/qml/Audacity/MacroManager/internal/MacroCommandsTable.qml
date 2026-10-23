/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

ColumnLayout {
    id: root

    property alias model: listView.model

    readonly property string dragMimeType: "application/x-audacity-macro-command"

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "MacroCommandsTable"
        direction: NavigationPanel.Vertical
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("macromanager", "Macro commands")
    }

    signal editRequested(int row)
    signal deleteRequested(int row)
    signal moveRequested(int from, int to)
    signal insertRequested(int row, string code, string title, bool isMacro)

    spacing: 0

    QtObject {
        id: prv

        readonly property int headerHeight: 32
        readonly property int sideMargin: 8
        readonly property int handleWidth: 32
        readonly property int stepWidth: 64
        readonly property int actionsWidth: 80
        readonly property int dropLineThickness: 2
        readonly property int emptyTextTopMargin: 24

        property int dropIndex: -1

        function dropIndexAt(y) {
            const contentY = y + listView.contentY
            const item = listView.itemAt(0, contentY)
            if (!item) {
                return y < 0 ? 0 : listView.count
            }

            const index = listView.indexAt(0, contentY)
            return contentY - item.y > item.height / 2 ? index + 1 : index
        }

        function dropLineY(index) {
            if (index >= 0 && index < listView.count) {
                const item = listView.itemAtIndex(index)
                return Boolean(item) ? item.y - listView.contentY : 0
            }

            return listView.contentHeight - listView.contentY
        }
    }

    Rectangle {
        Layout.fillWidth: true
        Layout.preferredHeight: prv.headerHeight

        color: ui.theme.backgroundTertiaryColor

        RowLayout {
            anchors.fill: parent
            anchors.rightMargin: prv.sideMargin

            spacing: 0

            Item {
                Layout.preferredWidth: prv.handleWidth
                Layout.fillHeight: true
            }

            SeparatorLine {}

            StyledTextLabel {
                Layout.preferredWidth: prv.stepWidth
                Layout.fillHeight: true

                text: qsTrc("macromanager", "Step")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Qt.AlignLeft
                leftPadding: prv.sideMargin
            }

            SeparatorLine {}

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.fillHeight: true

                text: qsTrc("macromanager", "Command")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Qt.AlignLeft
                leftPadding: prv.sideMargin
            }

            SeparatorLine {}

            StyledTextLabel {
                Layout.preferredWidth: prv.actionsWidth
                Layout.fillHeight: true

                text: qsTrc("macromanager", "Actions")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Qt.AlignLeft
                leftPadding: prv.sideMargin
            }
        }
    }

    SeparatorLine {}

    Item {
        Layout.fillWidth: true
        Layout.fillHeight: true

        StyledListView {
            id: listView

            anchors.fill: parent

            scrollBarPolicy: ScrollBar.AlwaysOn

            delegate: MacroCommandItem {
                required property int index

                width: ListView.view.width

                sideMargin: prv.sideMargin
                handleWidth: prv.handleWidth
                stepWidth: prv.stepWidth
                actionsWidth: prv.actionsWidth

                dragMimeType: root.dragMimeType
                dragRow: index

                navigationPanel: root.navigationPanel
                navigationRow: index

                canMoveUp: index > 0
                canMoveDown: index < listView.count - 1

                onEditRequested: {
                    root.editRequested(index)
                }

                onDeleteRequested: {
                    root.deleteRequested(index)
                }

                onMoveUpRequested: {
                    root.moveRequested(index, index - 1)
                }

                onMoveDownRequested: {
                    root.moveRequested(index, index + 1)
                }
            }
        }

        StyledTextLabel {
            anchors.top: parent.top
            anchors.topMargin: prv.emptyTextTopMargin
            anchors.left: parent.left
            anchors.right: parent.right

            visible: listView.count === 0

            text: qsTrc("macromanager", "Empty state")
        }

        DropArea {
            anchors.fill: parent

            keys: [root.dragMimeType]

            onPositionChanged: function (drag) {
                prv.dropIndex = prv.dropIndexAt(drag.y)
            }

            onExited: {
                prv.dropIndex = -1
            }

            onDropped: function (drop) {
                const dropIndex = prv.dropIndex
                prv.dropIndex = -1

                if (dropIndex < 0) {
                    return
                }

                const data = JSON.parse(drop.getDataAsString(root.dragMimeType))
                if (data.row >= 0) {
                    root.moveRequested(data.row, data.row < dropIndex ? dropIndex - 1 : dropIndex)
                } else {
                    root.insertRequested(dropIndex, data.code, data.title, Boolean(data.isMacro))
                }

                drop.accept()
            }
        }

        Rectangle {
            width: parent.width
            height: prv.dropLineThickness
            y: prv.dropLineY(prv.dropIndex)

            color: ui.theme.accentColor
            visible: prv.dropIndex >= 0
        }
    }
}
