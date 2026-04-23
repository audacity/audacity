import QtQuick
import QtQuick.Layouts
import Muse.Ui
import Muse.UiComponents

RoundedRectangle {
    id: root

    required property string mode
    property alias title: titleLabel.text

    property NavigationPanel navPanel: NavigationPanel {}

    color: ui.theme.backgroundSecondaryColor
    radius: 4
    border.color: ui.theme.strokeColor
    border.width: 1

    readonly property int valueFieldWidth: 64
    readonly property string navigationPrefix: root.mode === "tempo" ? "Tempo" : "Pitch"

    Column {
        anchors.fill: parent
        anchors.margins: 16
        spacing: 16

        StyledTextLabel {
            id: titleLabel
            font: ui.theme.bodyBoldFont
        }

        Loader {
            width: parent.width
            sourceComponent: root.mode === "tempo" ? tempoContent : pitchContent
        }
    }

    Component {
        id: tempoContent

        SlidingStretchTempoControls {
            width: parent ? parent.width : 0

            navPanel: root.navPanel
            navigationPrefix: root.navigationPrefix
            valueFieldWidth: root.valueFieldWidth
        }
    }

    Component {
        id: pitchContent

        SlidingStretchPitchControls {
            width: parent ? parent.width : 0

            navPanel: root.navPanel
            navigationPrefix: root.navigationPrefix
            valueFieldWidth: root.valueFieldWidth
        }
    }
}
