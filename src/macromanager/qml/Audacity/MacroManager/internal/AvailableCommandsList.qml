/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

StyledListView {
    id: root

    signal commandClicked(string code, string title)

    QtObject {
        id: prv

        readonly property int rowHeight: 32
        readonly property int sideMargin: 8
    }

    navigation.name: "AvailableCommandsList"
    navigation.direction: NavigationPanel.Vertical
    navigation.accessible.name: qsTrc("macromanager", "Available commands")

    delegate: ListItemBlank {
        id: item

        required property int index
        required property string code
        required property string title

        width: ListView.view.width
        height: prv.rowHeight

        navigation.panel: root.navigation
        navigation.row: item.index
        navigation.accessible.name: item.title

        onClicked: {
            root.commandClicked(item.code, item.title)
        }

        StyledTextLabel {
            anchors.fill: parent
            anchors.leftMargin: prv.sideMargin
            anchors.rightMargin: prv.sideMargin

            text: item.title
            horizontalAlignment: Qt.AlignLeft
            elide: Text.ElideRight
        }
    }
}
