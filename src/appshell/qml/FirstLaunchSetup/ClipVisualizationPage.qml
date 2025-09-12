/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0
import Audacity.ProjectScene 1.0

DoublePage {
    id: root

    title: clipStyleModel.pageTitle

    navigationPanel.direction: NavigationPanel.Vertical
    navigationPanel.accessible.name: qsTrc("appshell/gettingstarted", "Clip visualization options")
    navigationPanel.accessible.description: qsTrc("appshell/gettingstarted", "Choose how audio clips are displayed in the timeline")

    // Page-level accessibility information
    AccessibleItem {
        id: pageAccessibleInfo

        accessibleParent: root.navigationSection.accessible
        visualItem: root
        role: MUAccessible.Panel

        name: root.title
        description: qsTrc("appshell/gettingstarted", "Select your preferred clip visualization style. Preview is shown on the right.")
    }

    // Left side content
    leftContent: Column {
        anchors.fill: parent
        spacing: 0

        // Radio button options
        Column {
            spacing: 8
            width: parent.width

            Repeater {
                id: optionsRepeater

                model: clipStyleModel.clipStyles

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

                            navigation.name: "ClipStyleButton_" + index
                            navigation.panel: root.navigationPanel
                            navigation.column: 0
                            navigation.row: index
                            navigation.accessible.name: modelData.title
                            navigation.accessible.description: qsTrc("appshell/gettingstarted", "%1. %2").arg(modelData.description).arg(modelData.selected ? qsTrc("appshell/gettingstarted", "Currently selected") : qsTrc("appshell/gettingstarted", "Click to select this style"))

                            onToggled: {
                                clipStyleModel.selectClipStyle(modelData.style)
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
                                font: ui.theme.bodyFont
                                text: modelData.description
                            }
                        }
                    }
                    MouseArea {
                        anchors.fill: parent

                        onClicked: {
                            clipStyleModel.selectClipStyle(modelData.style)
                        }
                    }

                    // Accessibility item for the entire option
                    AccessibleItem {
                        accessibleParent: pageAccessibleInfo
                        visualItem: parent
                        role: MUAccessible.ListItem

                        name: modelData.title
                        description: qsTrc("appshell/gettingstarted", "%1. %2").arg(modelData.description).arg(modelData.selected ? qsTrc("appshell/gettingstarted", "Currently selected") : qsTrc("appshell/gettingstarted", "Available option"))
                    }
                }
            }
        }
    }

    // Right side content
    rightContent: Image {
        anchors.fill: parent
        fillMode: Image.PreserveAspectCrop
        smooth: true
        source: clipStyleModel.currentImagePath

        // Accessibility for the preview image
        AccessibleItem {
            accessibleParent: pageAccessibleInfo
            visualItem: parent
            role: MUAccessible.Information

            name: qsTrc("appshell/gettingstarted", "Clip visualization preview")
            description: qsTrc("appshell/gettingstarted", "Preview of how audio clips will appear with the selected visualization style")
        }
    }

    Component.onCompleted: {
        clipStyleModel.load()
    }

    SeparatorLine {
        anchors.horizontalCenter: parent.horizontalCenter
        orientation: Qt.Vertical
    }

    SeparatorLine {
        anchors.right: parent.right
        orientation: Qt.Vertical
    }
    ClipVisualizationPageModel {
        id: clipStyleModel
    }
}
