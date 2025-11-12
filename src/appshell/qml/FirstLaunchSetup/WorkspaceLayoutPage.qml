/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

DoublePage {
    id: root

    showRightContent: false
    title: workspaceModel.pageTitle

    navigationPanel.direction: NavigationPanel.Vertical
    navigationPanel.accessible.name: workspaceModel.navigationAccessibleName
    navigationPanel.accessible.description: workspaceModel.navigationAccessibleDescription

    // Page-level accessibility information
    AccessibleItem {
        id: pageAccessibleInfo

        accessibleParent: root.navigationSection.accessible
        visualItem: root
        role: MUAccessible.Panel

        name: root.title
        description: workspaceModel.pageAccessibleDescription
    }

    // Left side content
    leftContent: Column {
        anchors.fill: parent
        spacing: ui.theme.extra.space_16

        // Radio button options
        Column {
            spacing: ui.theme.extra.space_8
            width: parent.width

            Repeater {
                id: optionsRepeater

                model: workspaceModel.workspaces

                delegate: Rectangle {
                    border.color: modelData.selected ? ui.theme.accentColor : ui.theme.strokeColor
                    border.width: 1
                    color: "transparent"
                    height: 60
                    radius: 4
                    width: parent.width

                    Row {
                        anchors.bottomMargin: 12
                        anchors.left: parent.left
                        anchors.leftMargin: 12
                        anchors.rightMargin: 16
                        anchors.topMargin: 12
                        anchors.verticalCenter: parent.verticalCenter
                        spacing: ui.theme.extra.space_12

                        RoundedRadioButton {
                            anchors.verticalCenter: parent.verticalCenter
                            checked: modelData.selected

                            navigation.name: "Workspace_" + modelData.code
                            navigation.panel: root.navigationPanel
                            navigation.column: 0
                            navigation.row: index
                            navigation.accessible.name: modelData.title
                            navigation.accessible.description: workspaceModel.formatNavigationDescription(modelData.description, modelData.selected)

                            onToggled: {
                                workspaceModel.selectWorkspace(modelData.code)
                            }
                        }
                        Column {
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: ui.theme.extra.space_4

                            StyledTextLabel {
                                font: ui.theme.bodyBoldFont
                                text: modelData.title
                            }
                            StyledTextLabel {
                                anchors.left: parent.left
                                font: ui.theme.bodyFont
                                horizontalAlignment: Text.AlignLeft
                                text: modelData.description
                                wrapMode: Text.WordWrap
                            }
                        }
                    }
                    MouseArea {
                        anchors.fill: parent

                        onClicked: {
                            workspaceModel.selectWorkspace(modelData.code)
                        }
                    }

                    // Accessibility item for the entire workspace option
                    AccessibleItem {
                        accessibleParent: pageAccessibleInfo
                        visualItem: parent
                        role: MUAccessible.ListItem

                        name: modelData.title
                        description: workspaceModel.formatAccessibleDescription(modelData.description, modelData.selected)
                    }
                }
            }
        }

        // Additional info text
        StyledTextLabel {
            id: infoTextLabel
            font: ui.theme.bodyFont
            horizontalAlignment: Text.AlignLeft
            text: workspaceModel.additionalInfoText
            width: parent.width
            wrapMode: Text.WordWrap

            // Accessibility for the info text
            AccessibleItem {
                accessibleParent: pageAccessibleInfo
                visualItem: infoTextLabel
                role: MUAccessible.StaticText

                name: workspaceModel.additionalInfoAccessibleName
                description: infoTextLabel.text
            }
        }
    }

    Component.onCompleted: {
        workspaceModel.load()
    }

    WorkspaceLayoutPageModel {
        id: workspaceModel
    }
}
