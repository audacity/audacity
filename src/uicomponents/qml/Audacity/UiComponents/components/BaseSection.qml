/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

Column {
    id: root

    default property alias contentData: sectionContent.data

    width: parent.width
    spacing: 18

    property alias title: titleLabel.text
    property int columnWidth: 208
    property int columnSpacing: 12
    property int rowSpacing: 12

    property int navigationOrderStart: 0
    property int navigationOrderEnd: 0
    property NavigationPanel navigation: NavigationPanel {
        name: root.title
        direction: NavigationPanel.Vertical
        accessible.name: root.title
        enabled: root.enabled && root.visible

        onActiveChanged: function(active) {
            if (active) {
                root.forceActiveFocus()
            }
        }
    }

    StyledTextLabel {
        id: titleLabel
        font: ui.theme.bodyBoldFont
    }

    Column {
        id: sectionContent
        width: parent.width
        spacing: root.rowSpacing
    }
}

