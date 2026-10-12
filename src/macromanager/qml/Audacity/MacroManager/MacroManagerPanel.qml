/*
* Audacity: A Digital Audio Editor
*/
import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.MacroManager

import "internal"

Rectangle {
    id: root

    property alias navigationSection: navPanel.section
    property alias navigationOrderStart: navPanel.order

    color: ui.theme.backgroundTertiaryColor

    QtObject {
        id: prv

        readonly property int textTopMargin: 24
        readonly property int textSideMargin: 12
    }

    NavigationPanel {
        id: navPanel
        name: "MacroManagerPanel"
        direction: NavigationPanel.Horizontal
        enabled: root.enabled && root.visible
    }

    MacroManagerModel {
        id: macroManagerModel
    }

    Component.onCompleted: {
        macroManagerModel.init()
    }

    ColumnLayout {
        anchors.fill: parent

        spacing: 0

        MacroManagerHeader {
            Layout.fillWidth: true

            navigationPanel: navPanel
        }

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.topMargin: prv.textTopMargin
            Layout.leftMargin: prv.textSideMargin
            Layout.rightMargin: prv.textSideMargin

            wrapMode: Text.WordWrap

            text: qsTrc("macromanager", "You don't have any macros yet")
        }

        Item {
            Layout.fillHeight: true
        }
    }
}
