/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui 1.0
import Muse.UiComponents
import Muse.Workspace

import Audacity.UiComponents 1.0

BaseSection {
    id: root

    property var musicPreferencesModel: null

    navigation.direction: NavigationPanel.Both

    WorkspaceListModel {
        id: workspacesModel
    }

    Component.onCompleted: {
        workspacesModel.load()
    }

    ColumnLayout {
        width: parent.width

        StyledListView {
            id: workspacesList

            Layout.fillWidth: true
            Layout.preferredHeight: contentHeight + topMargin + bottomMargin

            leftMargin: 4
            topMargin: 4
            bottomMargin: 4

            spacing: 8

            model: workspacesModel

            interactive: false

            delegate: CheckBox {
                text: model.name

                width: parent.width

                checked: root.musicPreferencesModel.tempoDetectionWorkspaces.indexOf(model.name) !== -1

                navigation.name: model.name + "TempoDetectionBox"
                navigation.panel: root.navigation
                navigation.row: model.index

                onClicked: {
                    if (checked) {
                        musicPreferencesModel.removeFromTempoDetectionWorkspaces(model.name)
                    } else {
                        musicPreferencesModel.appendToTempoDetectionWorkspaces(model.name)
                    }
                }
            }
        }
    }
}
