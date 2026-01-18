/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents

import Audacity.TrackEdit 1.0

Column {
    id: root

    required property NavigationPanel navigation
    readonly property string title: qsTrc("trackedit/preferences", "Choose behavior when deleting a portion of a clip")
    required property color parentBackgroundColor

    property alias deleteBehavior: deleteBehaviorModel.deleteBehavior
    property alias closeGapBehavior: deleteBehaviorModel.closeGapBehavior

    signal newDeleteBehaviorRequested(int deleteBehavior)
    signal newCloseGapBehaviorRequested(int closeGapBehavior)

    spacing: 16
    width: imageRow.width

    Component.onCompleted: {
        root.navigation.direction = NavigationPanel.Both
        deleteBehaviorModel.init()
    }

    DeleteBehaviorPanelModel {
        id: deleteBehaviorModel
    }

    Row {
        id: imageRow

        spacing: 24

        Repeater {
            property var deleteBehaviors: deleteBehaviorModel.deleteBehaviors
            model: deleteBehaviors

            delegate: BehaviorChoice {
                text: modelData.text
                imageSource: modelData.imageSource
                addBorderToClipImageButton: deleteBehaviorModel.addBorderToClipImageButtons
                checked: root.deleteBehavior === modelData.value

                navigation.name: modelData.text
                navigation.panel: root.navigation
                navigation.row: 0
                navigation.column: model.index

                onToggled: {
                    newDeleteBehaviorRequested(modelData.value)
                }
            }
        }
    }

    Rectangle {
        width: parent.width
        height: gapBehaviorColumn.implicitHeight

        radius: 4
        color: parentBackgroundColor === ui.theme.backgroundPrimaryColor ? ui.theme.backgroundSecondaryColor : ui.theme.backgroundPrimaryColor
        border.color: ui.theme.strokeColor
        border.width: 1
        visible: deleteBehaviorModel.userMustChooseCloseGapBehavior

        Column {
            id: gapBehaviorColumn

            width: parent.width

            padding: 16
            spacing: 8

            StyledTextLabel {
                text: qsTrc("trackedit/preferences", "When closing the gap, do the following")
                width: parent.width
                horizontalAlignment: Text.AlignLeft

                font.bold: true
            }

            RadioButtonGroup {
                spacing: 8
                orientation: ListView.Vertical

                property var closeGapBehaviors: deleteBehaviorModel.closeGapBehaviors
                model: closeGapBehaviors

                delegate: RoundedRadioButton {
                    checked: root.closeGapBehavior === modelData.value
                    text: modelData.text
                    spacing: 8

                    navigation.name: modelData.text
                    navigation.panel: root.navigation
                    navigation.row: model.index + 1
                    navigation.column: 1

                    onToggled: {
                        newCloseGapBehaviorRequested(modelData.value)
                    }
                }
            }
        }
    }
}
