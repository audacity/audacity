/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

ColumnLayout {
    id: root

    enum Status {
        Queued,
        Processing,
        Complete
    }

    property alias model: listView.model

    spacing: 0

    QtObject {
        id: prv

        readonly property int headerHeight: 32
        readonly property int sideMargin: 8
        readonly property int statusWidth: 96
    }

    Rectangle {
        Layout.fillWidth: true
        Layout.preferredHeight: prv.headerHeight

        color: ui.theme.backgroundTertiaryColor

        RowLayout {
            anchors.fill: parent

            spacing: 0

            StyledTextLabel {
                Layout.fillWidth: true
                Layout.fillHeight: true

                text: qsTrc("macromanager", "File path")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Qt.AlignLeft
                leftPadding: prv.sideMargin
            }

            SeparatorLine {}

            StyledTextLabel {
                Layout.preferredWidth: prv.statusWidth
                Layout.fillHeight: true

                text: qsTrc("macromanager", "Status")
                font: ui.theme.bodyBoldFont
                horizontalAlignment: Qt.AlignLeft
                leftPadding: prv.sideMargin
            }
        }
    }

    SeparatorLine {}

    StyledListView {
        id: listView

        Layout.fillWidth: true
        Layout.fillHeight: true

        delegate: RunOnFilesItem {
            width: ListView.view.width

            sideMargin: prv.sideMargin
            statusWidth: prv.statusWidth
        }
    }
}
