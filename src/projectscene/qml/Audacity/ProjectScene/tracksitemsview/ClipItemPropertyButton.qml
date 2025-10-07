import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents
import Muse.GraphicalEffects

import Audacity.ProjectScene

FlatButton {
    id: root

    property color textColor: ui.theme.fontPrimaryColor

    anchors.verticalCenter: parent.verticalCenter
    height: 16
    margins: 2

    transparent: true
    orientation: Qt.Horizontal

    contentItem: RowLayout {
        width: Math.min(implicitWidth, root.width)
        spacing: 2

        StyledIconLabel {
            Layout.alignment: Qt.AlignVCenter
            Layout.leftMargin: 2
            Layout.preferredWidth: 8
            Layout.preferredHeight: 8
            iconCode: root.icon
            color: root.iconColor
            font.pixelSize: 12
        }

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.rightMargin: 2
            Layout.alignment: Qt.AlignVCenter
            text: root.text
            color: root.textColor
        }
    }
}
