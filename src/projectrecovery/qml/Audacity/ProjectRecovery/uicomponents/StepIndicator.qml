import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

ColumnLayout {
    id: root

    property int count: 1
    //! 1 for the first step
    property int current: 1

    spacing: 8

    StyledTextLabel {
        Layout.fillWidth: true
        text: qsTrc("uicomponents", "Step %1 of %2").arg(root.current).arg(root.count)
        horizontalAlignment: Text.AlignLeft
    }

    RowLayout {
        Layout.fillWidth: true
        spacing: 4

        Repeater {
            model: root.count

            Rectangle {
                required property int index

                Layout.fillWidth: true
                Layout.preferredHeight: 4
                radius: 2
                color: index < root.current ? ui.theme.extra["progress_indicator_color"] : ui.theme.extra["progress_track_color"]
            }
        }
    }
}
