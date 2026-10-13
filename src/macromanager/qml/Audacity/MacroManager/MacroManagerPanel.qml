/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import Audacity.MacroManager

import "internal"

Rectangle {
    id: root

    property NavigationSection navigationSection: null
    property int navigationOrderStart: 0

    color: ui.theme.backgroundTertiaryColor

    QtObject {
        id: prv

        readonly property int emptyTextTopMargin: 24
        readonly property int emptyTextSideMargin: 12
    }

    NavigationPanel {
        id: navPanel
        name: "MacroManagerPanel"
        direction: NavigationPanel.Horizontal
        section: root.navigationSection
        order: root.navigationOrderStart
        enabled: root.enabled && root.visible
    }

    NavigationPanel {
        id: listNavPanel
        name: "MacroManagerList"
        direction: NavigationPanel.Both
        section: root.navigationSection
        order: root.navigationOrderStart + 1
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

            onNewMacroRequested: {
                macroManagerModel.newMacro()
            }
        }

        SeparatorLine {}

        StyledTextLabel {
            Layout.fillWidth: true
            Layout.topMargin: prv.emptyTextTopMargin
            Layout.leftMargin: prv.emptyTextSideMargin
            Layout.rightMargin: prv.emptyTextSideMargin

            visible: macroManagerModel.isEmpty

            wrapMode: Text.WordWrap

            text: qsTrc("macromanager", "You don't have any macros yet")
        }

        StyledListView {
            Layout.fillWidth: true
            Layout.fillHeight: true

            visible: !macroManagerModel.isEmpty

            model: macroManagerModel
            spacing: 0

            delegate: MacroItem {
                required property int index

                width: ListView.view.width

                navigationPanel: listNavPanel
                navigationRow: index

                onRunRequested: {
                    macroManagerModel.run(index)
                }

                onRenameRequested: {
                    macroManagerModel.rename(index)
                }

                onDeleteRequested: {
                    macroManagerModel.remove(index)
                }
            }
        }

        Item {
            Layout.fillHeight: true

            visible: macroManagerModel.isEmpty
        }
    }
}
