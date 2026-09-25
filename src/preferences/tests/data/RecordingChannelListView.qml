/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import "../../qml/Audacity/Preferences/internal"

ListView {
    width: 200
    height: 180
    spacing: 8

    model: RecordingChannelListModel {
        groups: apiModel.inputChannelGroups
    }

    delegate: Item {
        required property var model
        required property int index

        readonly property bool checked: model.checked
        height: model.sectionStart ? 60 : 32
        width: ListView.view.width
    }

    function scrollToGroup(index) {
        positionViewAtIndex(index, ListView.Center)
        forceLayout()
        return itemAtIndex(index)
    }
}
