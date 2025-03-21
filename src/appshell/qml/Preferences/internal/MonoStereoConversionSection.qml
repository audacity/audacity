
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Mono & stereo conversion")

    property bool askBeforeConverting: true

    Column {
        width: parent.width
        spacing: 24

        CheckBox {
            id: checkbox

            width: parent.width

            text: qsTrc("appshell/preferences", "Always convert to mono without prompt")

            checked: !root.askBeforeConverting

            navigation.name: "StereoToMonoBox"
            navigation.panel: root.navigation

            onClicked: {
                root.askBeforeConverting = !root.askBeforeConverting
            }
        }
    }
}
