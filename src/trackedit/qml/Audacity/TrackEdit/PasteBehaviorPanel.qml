/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.TrackEdit 1.0

Column {
    id: root

    required property NavigationPanel navigation
    readonly property string title: qsTrc("appshell/preferences", "Choose behavior when pasting audio")
    required property color parentBackgroundColor

    property alias pasteBehavior: pasteBehaviorModel.pasteBehavior
    property alias pasteInsertBehavior: pasteBehaviorModel.pasteInsertBehavior

    signal newPasteBehaviorRequested(int pasteBehavior)
    signal newPasteInsertBehaviorRequested(int pasteInsertBehavior)

    spacing: ui.theme.extra.space_16
    width: imageRow.width

    Component.onCompleted: {
        root.navigation.direction = NavigationPanel.Both
        pasteBehaviorModel.init()
    }

    PasteBehaviorPanelModel {
        id: pasteBehaviorModel
    }

    Row {
        id: imageRow

        spacing: ui.theme.extra.space_24

        Repeater {
            property var pasteBehaviors: pasteBehaviorModel.pasteBehaviors
            model: pasteBehaviors

            delegate: BehaviorChoice {
                text: modelData.text
                imageSource: modelData.imageSource
                addBorderToClipImageButton: pasteBehaviorModel.addBorderToClipImageButtons
                checked: root.pasteBehavior === modelData.value

                navigation.name: modelData.text
                navigation.panel: root.navigation
                navigation.row: 0
                navigation.column: model.index

                onToggled: {
                    newPasteBehaviorRequested(modelData.value)
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
        visible: pasteBehaviorModel.userMustChoosePasteInsertBehavior

        Column {
            id: gapBehaviorColumn

            width: parent.width

            padding: 16
            spacing: ui.theme.extra.space_8

            StyledTextLabel {
                text: qsTrc("trackedit/preferences", "When making room for pasted audio, do the following")
                width: parent.width
                horizontalAlignment: Text.AlignLeft

                font.bold: true
            }

            RadioButtonGroup {
                spacing: ui.theme.extra.space_8
                orientation: ListView.Vertical

                property var pasteInsertBehaviors: pasteBehaviorModel.pasteInsertBehaviors
                model: pasteInsertBehaviors

                delegate: RoundedRadioButton {
                    checked: root.pasteInsertBehavior === modelData.value
                    text: modelData.text
                    spacing: ui.theme.extra.space_8

                    navigation.name: modelData.text
                    navigation.panel: root.navigation
                    navigation.row: model.index + 1
                    navigation.column: 1

                    onToggled: {
                        newPasteInsertBehaviorRequested(modelData.value)
                    }
                }
            }
        }
    }
}
