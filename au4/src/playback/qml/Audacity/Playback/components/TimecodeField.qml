/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0

ListItemBlank {
    id: root

    property var value: null
    property bool isEditable: false

    width: isEditable ? 12 : (value === " " ? 1 : symbolField.implicitWidth)
    height: 28

    mouseArea.enabled: isEditable

    opacity: enabled ? 1.0 : ui.theme.itemOpacityDisabled

    StyledTextLabel {
        id: symbolField

        anchors.verticalCenter: parent.verticalCenter
        anchors.horizontalCenter: parent.horizontalCenter

        text: root.value
        opacity: root.isEditable ? 1 : 0.75
        font: ui.theme.tabBoldFont
        color: ui.theme.fontSecondaryColor
    }
}
