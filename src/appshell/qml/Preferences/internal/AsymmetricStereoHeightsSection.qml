/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Muse.GraphicalEffects

import Audacity.ProjectScene

BaseSection {
    id: root

    title: qsTrc("appshell/preferences", "Asymmetric stereo heights")

    navigation.direction: NavigationPanel.Both

    property var editPreferencesModel: null
    navigationOrderEnd: workspaceSection.navigation.order

    rowSpacing: 16

    Rectangle {
        id: svgImg

        width: 220
        height: 64

        layer.enabled: true
        layer.effect: EffectOpacityMask {
            maskSource: RoundedRectangle {
                width: svgImg.width
                height: svgImg.height
                radius: 5
            }
        }

        Image {
            source: "qrc:/resources/AsymmetricStereoHeights.svg"

            width: svgImg.width
            height: svgImg.height

            Image {
                source: "qrc:/resources/StereoHeightsCursor.png"

                width: 19
                height: 21

                x: 68
                y: 27
            }
        }
    }

    StyledTextLabel {
        text: qsTrc("appshell/preferences", "Dragging on the center line may adjust the height of the channel:")
    }

    RadioButtonGroup {

        width: parent.width
        height: workspaceSection.visible ? 80 + workspaceSection.height : 80

        spacing: root.rowSpacing
        orientation: Qt.Vertical

        Column {
            width: parent.width
            spacing: root.columnSpacing

            RoundedRadioButton {

                checked: editPreferencesModel.stereoHeightsPref == AsymmetricStereoHeights.ALWAYS
                text: qsTrc("appshell/preferences", "Always")

                navigation.name: "AlwaysRadioBtn"
                navigation.panel: root.navigation
                navigation.row: 0

                onToggled: {
                    editPreferencesModel.setStereoHeightsPref(AsymmetricStereoHeights.ALWAYS)
                }
            }

            RoundedRadioButton {
                id: workspaceRadioBtn

                checked: editPreferencesModel.stereoHeightsPref == AsymmetricStereoHeights.WORKSPACE_DEPENDENT
                text: qsTrc("appshell/preferences", "Depending on workspace")

                navigation.name: "WorkspaceDependentRadioBtn"
                navigation.panel: root.navigation
                navigation.row: 1

                onToggled: {
                    editPreferencesModel.setStereoHeightsPref(AsymmetricStereoHeights.WORKSPACE_DEPENDENT)
                }
            }

            WorkspacesAsymmetricChannelsSection {
                id: workspaceSection

                x: 30

                visible: workspaceRadioBtn.checked

                editPreferencesModel: root.editPreferencesModel

                navigation.section: root.navigation.section
                navigation.order: root.navigation.order + 1
            }

            RoundedRadioButton {
                checked: editPreferencesModel.stereoHeightsPref == AsymmetricStereoHeights.NEVER
                text: qsTrc("appshell/preferences", "Never")

                navigation.name: "NeverRadioBtn"
                navigation.panel: root.navigation
                navigation.row: 3

                onToggled: {
                    editPreferencesModel.setStereoHeightsPref(AsymmetricStereoHeights.NEVER)
                }
            }
        }
    }
}
