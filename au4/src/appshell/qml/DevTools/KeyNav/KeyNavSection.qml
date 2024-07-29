/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0


Rectangle {
    id: root

    property alias keynavSection: keynavsec
    property alias sectionName: keynavsec.name
    property alias sectionOrder: keynavsec.order
    property alias active: keynavsec.active

    default property alias content: contentItem.data

    NavigationFocusBorder { navigationCtrl: keynavsec }

    NavigationSection {
        id: keynavsec
        onActiveChanged: {
            console.debug("KeyNavSection.qml active: " + keynavsec.active)
            if (keynavsec.active) {
                root.forceActiveFocus()

            }
        }
    }

    Item {
        id: contentItem
        anchors.fill: parent
    }

    FlatButton {
        id: btn
        anchors.right: parent.right
        width: 20
        height: 20
        text: keynavsec.enabled ? "ON" : "OFF"
        onClicked: keynavsec.enabled = !keynavsec.enabled
    }
}
