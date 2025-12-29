/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui 1.0
import Muse.UiComponents

StyledDialogView {
    id: root

    resizable: false

    required property int instanceId

    property NavigationPanel navigationPanel: NavigationPanel {
        name: root.title
        enabled: root.isOpened
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
    }
}
