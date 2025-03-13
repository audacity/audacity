/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.Workspace

BaseSection {
    id: root

    property var editPreferencesModel: null

    navigation.direction: NavigationPanel.Both

    WorkspaceListModel {
        id: workspacesModel
    }

    Component.onCompleted: {
        workspacesModel.load()
    }

    Column {
        width: parent.width
        height: workspacesList.height

        StyledListView {
            id: workspacesList

            width: parent.width
            height: contentHeight + topMargin + bottomMargin

            spacing: 8
            leftMargin: 2
            topMargin: 2
            bottomMargin: 2

            model: workspacesModel

            interactive: false

            delegate: CheckBox {
                text: model.name

                width: parent.width

                checked: root.editPreferencesModel.asymmetricWorkspaces.indexOf(model.name) != -1

                navigation.name: model.name + "Box"
                navigation.panel: root.navigation
                navigation.row: model.index

                onClicked: {
                    if (checked) {
                        editPreferencesModel.removeFromAsymmetricWorkspaces(model.name)
                    } else {
                        editPreferencesModel.appendToAsymmetricWorkspaces(model.name)
                    }
                }
            }
        }
    }
}
