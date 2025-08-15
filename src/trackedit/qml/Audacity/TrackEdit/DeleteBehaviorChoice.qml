/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.TrackEdit 1.0

Column {
    id: root

    required property bool isCloseGapBehavior
    property alias checked: radioBtn.checked

    signal toggled()

    spacing: 16

    ClipImageButton {
        width: 196
        height: 88

        radius: 5
        source: "qrc:/resources/Colorful.svg"

        onClicked: {
            root.toggled()
        }
    }

    RoundedRadioButton {
        id: radioBtn
        text: root.isCloseGapBehavior ? qsTrc("trackedit/preferences", "Close gap (ripple)") : qsTrc("trackedit/preferences", "Leave gap")
        onToggled: {
            root.toggled()
        }
    }
}
