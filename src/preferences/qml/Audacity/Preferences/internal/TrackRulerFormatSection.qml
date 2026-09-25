/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui
import Muse.UiComponents

import Audacity.UiComponents

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Vertical rulers")

    property alias rulerTypeModel: rulerTypeBox.model
    property int defaultRulerType: 0

    signal defaultRulerTypeChangeRequested(int rulerType)

    ComboBoxWithTitle {
        id: rulerTypeBox

        title: qsTrc("appshell/preferences", "Default ruler format:")
        columnWidth: root.columnWidth

        control.textRole: "title"
        control.valueRole: "value"

        currentIndex: control.indexOfValue(root.defaultRulerType)

        navigation.name: "DefaultRulerFormatBox"
        navigation.panel: root.navigation
        navigation.row: 0
        navigation.column: 0

        onValueEdited: function (newIndex, newValue) {
            root.defaultRulerTypeChangeRequested(newValue)
        }
    }
}
