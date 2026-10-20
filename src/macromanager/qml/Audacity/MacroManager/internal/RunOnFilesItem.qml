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

    required property string filePath
    required property int status

    property int sideMargin: 0
    property int statusWidth: 0

    readonly property string statusText: {
        switch (root.status) {
        case RunOnFilesTable.Queued:
            return qsTrc("macromanager", "Queued")
        case RunOnFilesTable.Processing:
            return qsTrc("macromanager", "Processing")
        case RunOnFilesTable.Complete:
            return qsTrc("macromanager", "Complete")
        }

        return ""
    }

    implicitHeight: prv.height

    color: hoverHandler.hovered ? ui.theme.extra["background_table_item_hover_color"] : ui.theme.extra["background_table_item_idle_color"]

    QtObject {
        id: prv

        readonly property int height: 40
    }

    HoverHandler {
        id: hoverHandler
    }

    SeparatorLine {
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
    }

    RowLayout {
        anchors.fill: parent

        spacing: 0

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.fillHeight: true

            text: root.filePath
            horizontalAlignment: Qt.AlignLeft
            elide: Text.ElideMiddle
            leftPadding: root.sideMargin
        }

        SeparatorLine {}

        StyledTextLabel {
            Layout.preferredWidth: root.statusWidth
            Layout.fillHeight: true

            text: root.statusText
            horizontalAlignment: Qt.AlignLeft
            leftPadding: root.sideMargin
        }
    }
}
