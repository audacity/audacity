/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

RowLayout {
    id: root

    property bool isMovingUpAvailable: false
    property bool isMovingDownAvailable: false
    property bool isRemovingAvailable: false
    property bool isAddingAvailable: value
    property bool isInstrumentSelected: false

    property alias navigation: keynavSub

    signal addRequested()
    signal moveUpRequested()
    signal moveDownRequested()
    signal removingRequested()

    spacing: 6

    focus: true

    Keys.onShortcutOverride: function(event) {
        if (event.key === Qt.Key_Delete) {
            root.removingRequested()
        }
    }

    NavigationPanel {
        id: keynavSub
        name: "TracksHeader"
        enabled: root.enabled && root.visible
    }

    FlatButton {
        Layout.fillWidth: true

        navigation.name: "Add"
        navigation.panel: keynavSub
        navigation.order: 1
        accessible.name: qsTrc("projectscene", "Add Track")

        text: qsTrc("projectscene", "Add")

        enabled: root.isAddingAvailable

        onClicked: {
            root.addRequested()
        }
    }

    FlatButton {
        Layout.preferredWidth: width

        navigation.name: "Up"
        navigation.panel: keynavSub
        navigation.order: 2

        toolTipTitle: qsTrc("projectscene", "Move selected track up")

        enabled: root.isMovingUpAvailable

        icon: IconCode.ARROW_UP

        onClicked: {
            root.moveUpRequested()
        }
    }

    FlatButton {
        Layout.preferredWidth: width

        navigation.name: "Down"
        navigation.panel: keynavSub
        navigation.order: 3

        toolTipTitle: qsTrc("projectscene", "Move selected track down")

        enabled: root.isMovingDownAvailable

        icon: IconCode.ARROW_DOWN

        onClicked: {
            root.moveDownRequested()
        }
    }

    FlatButton {
        Layout.preferredWidth: width

        navigation.name: "Remove"
        navigation.panel: keynavSub
        navigation.order: 4

        toolTipTitle: qsTrc("projectscene", "Remove track instruments")

        enabled: root.isRemovingAvailable

        icon: IconCode.DELETE_TANK

        onClicked: {
            root.removingRequested()
        }
    }
}
