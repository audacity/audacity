/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.UiComponents

import Audacity.ProjectScene

TrackItem {
    id: root

    signal addLabelToSelectionRequested()

    extraControlsComponent: Component {
        FlatButton {
            width: parent.width
            height: 24

            text: qsTrc("projectscene", "Add label")

            opacity: root.collapsed ? 0 : 1
            visible: opacity !== 0
            Behavior on opacity { OpacityAnimator { duration: 100 } }

            navigation.panel: root.navigation.panel
            navigation.order: root.extraControlsNavigationStart
            navigation.enabled: !root.collapsed

            onClicked: {
                root.addLabelToSelectionRequested()
            }
        }
    }
}
