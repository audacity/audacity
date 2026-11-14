/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Muse.GraphicalEffects 1.0
import Audacity.Project 1.0

ListItemBlank {
    id: root

    required property var item
    property var columns: []
    property real columnsContentX: 0
    property real columnsMinWidth: 0
    signal columnScrollRequested(real columnX, real columnWidth)

    function scrollColumnIntoView(remainingColumnIndex) {
        var x = 0

        if (remainingColumnIndex + 1 >= root.columns.length) {
            return
        }

        for (var i = 0; i < remainingColumnIndex; i++) {
            x += root.columns[i + 1].width + root.columnSpacing
        }

        var colWidth = root.columns[remainingColumnIndex + 1].width
        root.columnScrollRequested(x, colWidth)
    }

    property real itemInset: 12
    property real columnSpacing: 44

    property alias showBottomBorder: bottomBorder.visible

    implicitHeight: 64

    navigation.accessible.name: root.item.name ?? ""
    navigation.onActiveChanged: {
        if (navigation.active) {
            root.scrollIntoView()
        }
    }

    focusBorder.anchors.bottomMargin: bottomBorder.visible ? bottomBorder.height : 0

    RowLayout {
        anchors.fill: parent

        anchors.leftMargin: root.itemInset
        anchors.rightMargin: root.itemInset

        spacing: root.columnSpacing

        Loader {
            readonly property var columnData: root.columns.length > 0 ? root.columns[0] : null

            active: columnData !== null

            Layout.preferredWidth: columnData ? columnData.width : 0
            Layout.minimumWidth: columnData ? columnData.width : 0
            Layout.fillWidth: true

            readonly property ProjectListItem listItem: root
            readonly property var item: root.item
            readonly property NavigationPanel navigationPanel: root.navigation.panel
            readonly property int navigationRow: root.navigation.row
            readonly property int navigationColumnStart: 100

            sourceComponent: columnData ? columnData.delegate : null
        }

        Item {
            Layout.preferredHeight: parent.height
            Layout.preferredWidth: root.columnsMinWidth
            Layout.maximumWidth: root.columnsMinWidth
            Layout.minimumWidth: 0
            Layout.fillWidth: true

            clip: true

            RowLayout {
                id: columnsRow
                x: -root.columnsContentX
                width: Math.max(parent.width, root.columnsMinWidth)
                height: parent.height

                spacing: root.columnSpacing

                Repeater {
                    id: columnsRepeater

                    model: Math.max(root.columns.length - 1, 0)

                    delegate: Loader {
                        readonly property var columnData: root.columns[model.index + 1]

                        Layout.preferredWidth: columnData.width
                        Layout.minimumWidth: columnData.width

                        readonly property ProjectListItem listItem: root
                        readonly property var item: root.item
                        readonly property NavigationPanel navigationPanel: root.navigation.panel
                        readonly property int navigationRow: root.navigation.row
                        readonly property int navigationColumnStart: 100 * (model.index + 2)
                        readonly property int remainingColumnIndex: model.index

                        sourceComponent: columnData.delegate
                    }
                }
            }
        }
    }

    SeparatorLine {
        id: bottomBorder
        anchors.bottom: parent.bottom
    }
}
