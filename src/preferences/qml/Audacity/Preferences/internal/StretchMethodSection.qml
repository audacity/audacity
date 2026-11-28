/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Clip stretching")

    navigation.direction: NavigationPanel.Both

    property alias stretchMethodModel: methodBox.model
    property int stretchMethod: 0

    signal stretchMethodChangeRequested(int method)

    StyledTextLabel {
        text: qsTrc("appshell/preferences", "How new clips are sped up or slowed down. “Change speed” plays the audio faster or slower, so the pitch goes up or down with it; the others keep the pitch. You can change it for each clip from its menu.")
        width: parent.width
        horizontalAlignment: Text.AlignLeft
        wrapMode: Text.WordWrap
    }

    ComboBoxWithTitle {
        id: methodBox

        title: qsTrc("appshell/preferences", "Default stretching method:")
        columnWidth: root.columnWidth

        control.textRole: "title"
        control.valueRole: "value"

        currentIndex: control.indexOfValue(root.stretchMethod)

        navigation.name: "DefaultStretchMethodBox"
        navigation.panel: root.navigation
        navigation.row: 0
        navigation.column: 0

        onValueEdited: function (newIndex, newValue) {
            root.stretchMethodChangeRequested(newValue)
        }
    }
}
