/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Controls
import QtQuick.Layouts

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

    ColumnLayout {
        width: parent.width

        StyledListView {
            id: workspacesList

            Layout.fillWidth: true
            Layout.preferredHeight: contentHeight
            Layout.leftMargin: 2
            Layout.topMargin: 2
            Layout.bottomMargin: 2

            spacing: ui.theme.extra.spacing_m

            model: workspacesModel

            interactive: false

            delegate: CheckBox {
                text: model.name

                width: parent.width

                checked: root.editPreferencesModel.asymmetricWorkspaces.indexOf(model.name) !== -1

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
