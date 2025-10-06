/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

TrackItem {
    id: root

    extraControlsComponent: Component {
        FlatButton {
            width: parent.width
            height: 24

            opacity: root.collapsed ? 0 : 1
            visible: opacity !== 0
            Behavior on opacity { OpacityAnimator { duration: 100 } }

            text: qsTrc("projectscene", "Add label")

            onClicked: {
                // root.addLabelRequested()
            }
        }
    }
}


