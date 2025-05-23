/*
* Audacity: A Digital Audio Editor
*/
import QtQuick

import Muse.Ui 1.0
import Muse.UiComponents 1.0

StyledDialogView {
    id: root

    property int implicitWidth: 0
    property int implicitHeight: 0
    property int minimumWidth: 0
    property int minimumHeight: 0

    contentWidth: Math.max(implicitWidth, minimumWidth)
    contentHeight: Math.max(implicitHeight, minimumHeight)

    property NavigationPanel navigationPanel: NavigationPanel {
        name: root.title
        enabled: root.isOpened
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
    }
}
