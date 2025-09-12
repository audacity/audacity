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
    navigationPanel.accessible.name: qsTrc("appshell/gettingstarted", "Workspace layout options")
    navigationPanel.accessible.description: qsTrc("appshell/gettingstarted", "Choose your preferred workspace layout for the Audacity interface")

    // Page-level accessibility information
    AccessibleItem {
        id: pageAccessibleInfo

        accessibleParent: root.navigationSection.accessible
        visualItem: root
        role: MUAccessible.Panel

        name: root.title
        description: qsTrc("appshell/gettingstarted", "Select a workspace layout that suits your workflow. You can change this later.")
    }

    // Left side content
    leftContent: Column {
        anchors.fill: parent
        spacing: 16

        // Radio button options
        Column {
            spacing: 8
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
                        spacing: 12

                        RoundedRadioButton {
                            anchors.verticalCenter: parent.verticalCenter
                            checked: modelData.selected

                            navigation.name: "Workspace_" + modelData.code
                            navigation.panel: root.navigationPanel
                            navigation.column: 0
                            navigation.row: index
                            navigation.accessible.name: modelData.title
                            navigation.accessible.description: qsTrc("appshell/gettingstarted", "%1. %2").arg(modelData.description).arg(modelData.selected ? qsTrc("appshell/gettingstarted", "Currently selected") : qsTrc("appshell/gettingstarted", "Click to select this workspace"))

                            onToggled: {
                                workspaceModel.selectWorkspace(modelData.code)
                            }
                        }
                        Column {
                            anchors.verticalCenter: parent.verticalCenter
                            spacing: 4

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
                        description: qsTrc("appshell/gettingstarted", "%1. %2").arg(modelData.description).arg(modelData.selected ? qsTrc("appshell/gettingstarted", "Currently selected") : qsTrc("appshell/gettingstarted", "Available workspace"))
                    }
                }
            }
        }

        // Additional info text
        StyledTextLabel {
            id: infoTextLabel
            font: ui.theme.bodyFont
            horizontalAlignment: Text.AlignLeft
            text: qsTrc("appshell/gettingstarted", "You can change between these layouts at any time using our new 'workspaces' feature.")
            width: parent.width
            wrapMode: Text.WordWrap

            // Accessibility for the info text
            AccessibleItem {
                accessibleParent: pageAccessibleInfo
                visualItem: infoTextLabel
                role: MUAccessible.StaticText

                name: qsTrc("appshell/gettingstarted", "Additional information")
                description: infoTextLabel.text
            }
        }
    }

    // Right side content
    rightContent: Image {
        anchors.fill: parent
        asynchronous: true
        cache: true
        fillMode: Image.PreserveAspectFit
        mipmap: true
        smooth: false
        source: workspaceModel.currentImagePath

        // Accessibility for the workspace preview image
        AccessibleItem {
            accessibleParent: pageAccessibleInfo
            visualItem: parent
            role: MUAccessible.Information

            name: qsTrc("appshell/gettingstarted", "Workspace layout preview")
            description: qsTrc("appshell/gettingstarted", "Preview of the selected workspace layout showing the arrangement of interface elements")
        }
    }

    Component.onCompleted: {
        workspaceModel.load()
    }

    SeparatorLine {
        anchors.horizontalCenter: parent.horizontalCenter
        orientation: Qt.Vertical
        visible: showRightContent
    }

    SeparatorLine {
        anchors.right: parent.right
        orientation: Qt.Vertical
        visible: showRightContent
    }
    WorkspaceLayoutPageModel {
        id: workspaceModel
    }
}
