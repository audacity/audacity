/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Rectangle {

    id: root

    property alias keynavSection: keynavsec.section
    property alias subsectionName: keynavsec.name
    property alias subsectionOrder: keynavsec.order

    height: 40
    width: btns.childrenRect.width

    NavigationFocusBorder { navigationCtrl: keynavsec }

    signal clicked(string info)

    function doClicked(control) {
        var info = "sub: " + root.subsectionName + ", control: " + control
        console.log(info)
        root.clicked(info)
    }

    NavigationPanel {
        id: keynavsec

        onActiveChanged: {
            if (keynavsec.active) {
                root.forceActiveFocus()
            }
        }
    }

    Row {
        id: btns
        anchors.fill: parent
        spacing: 8

        FlatButton {
            id: btn1
            navigation.panel: keynavsec
            navigation.order: 1
            anchors.verticalCenter: parent.verticalCenter
            height: 24
            width: 24
            text: "C1"
            onClicked: root.doClicked(text)
        }

        FlatButton {
            id: btn2
            navigation.panel: keynavsec
            navigation.order: 2
            anchors.verticalCenter: parent.verticalCenter
            height: 24
            width: 24
            text: "C2"
            onClicked: root.doClicked(text)
        }

        FlatButton {
            id: btn3
            navigation.panel: keynavsec
            navigation.order: 3
            anchors.verticalCenter: parent.verticalCenter
            height: 24
            width: 24
            text: "C3"
            onClicked: root.doClicked(text)
        }
    }
}
