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

    required property var project
    property alias columns: columnsRepeater.model

    property real itemInset: 12
    property real columnSpacing: 44

    implicitHeight: 64

    navigation.accessible.name: root.project.title ?? ""
    navigation.onActiveChanged: {
        if (navigation.active) {
            root.scrollIntoView()
        }
    }

    focusBorder.anchors.bottomMargin: 0

    RowLayout {
        anchors.fill: parent
        anchors.leftMargin: root.itemInset
        anchors.rightMargin: root.itemInset

        spacing: root.columnSpacing

        StyledTextLabel {
            Layout.preferredWidth: 71
            Layout.preferredHeight: 40

            text: root.project.name ?? ""
            font: ui.theme.largeBodyFont
            horizontalAlignment: Text.AlignLeft
        }

        Repeater {
            id: columnsRepeater

            delegate: Loader {
                Layout.preferredWidth: modelData.width(parent.width)

                // These properties are here to give the delegate access to them
                readonly property AudioListItem listItem: root
                readonly property var project: root.project
                readonly property NavigationPanel navigationPanel: root.navigation.panel
                readonly property int navigationRow: root.navigation.row
                readonly property int navigationColumnStart: 100 * (model.index + 1)

                sourceComponent: modelData.delegate
            }
        }
    }
}
