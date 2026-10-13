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

    property alias model: listView.model

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "MacroCommandsTable"
        direction: NavigationPanel.Both
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("macromanager", "Macro commands")
    }

    signal editRequested(int row)
    signal deleteRequested(int row)

    spacing: 0

    QtObject {
        id: prv

        readonly property int headerHeight: 32
        readonly property int sideMargin: 8
        readonly property int handleWidth: 32
        readonly property int stepWidth: 64
        readonly property int actionsWidth: 80
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

    StyledListView {
        id: listView

        Layout.fillWidth: true
        Layout.fillHeight: true

        delegate: MacroCommandItem {
            required property int index

            width: ListView.view.width

            sideMargin: prv.sideMargin
            handleWidth: prv.handleWidth
            stepWidth: prv.stepWidth
            actionsWidth: prv.actionsWidth

            navigationPanel: root.navigationPanel
            navigationRow: index

            onEditRequested: {
                root.editRequested(index)
            }

            onDeleteRequested: {
                root.deleteRequested(index)
            }
        }
    }
}
