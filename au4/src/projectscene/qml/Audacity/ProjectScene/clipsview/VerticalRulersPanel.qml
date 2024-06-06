import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {
    id: verticalRulersPanel

    width: 48
    color: ui.theme.backgroundPrimaryColor
    visible: false

    //! TODO AU4: create as many rulers as there are tracks

    Rectangle {
        id: tmpLeftBorder

        width: 1
        height: parent.height
        color: ui.theme.strokeColor
    }

    Text {
        id: tmpLabel

        x: -40
        y: 80
        text: qsTr("Vertical rulers mock")
        rotation : 270
    }

}
