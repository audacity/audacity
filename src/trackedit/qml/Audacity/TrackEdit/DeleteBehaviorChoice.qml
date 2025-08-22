/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.TrackEdit 1.0

Column {
    id: root

    property alias imageSource: imageBtn.source
    property alias text: radioBtn.text
    property alias checked: radioBtn.checked
    property alias navigation: radioBtn.navigation

    signal toggled()

    spacing: 16

    ClipImageButton {
        id: imageBtn

        width: 196
        height: 88

        radius: 5

        onClicked: {
            root.toggled()
        }
    }

    RoundedRadioButton {
        id: radioBtn
        onToggled: {
            root.toggled()
        }
    }
}
