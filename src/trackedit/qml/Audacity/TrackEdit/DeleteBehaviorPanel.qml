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
    readonly property string title: qsTrc("appshell/preferences", "Choose behavior when deleting a portion of a clip")

    required property int deleteBehavior
    required property int closeGapBehavior
    required property color parentBackgroundColor

    signal newDeleteBehaviorRequested(int deleteBehavior)
    signal newCloseGapBehaviorRequested(int closeGapBehavior)

    spacing: 16
    width: imageRow.width

    Row {
        id: imageRow

        spacing: 24

        DeleteBehaviorChoice {
            isCloseGapBehavior: false
            checked: root.deleteBehavior === DeleteBehavior.LeaveGap
            onToggled: {
                newDeleteBehaviorRequested(DeleteBehavior.LeaveGap)
            }
        }

        DeleteBehaviorChoice {
            isCloseGapBehavior: true
            checked: root.deleteBehavior === DeleteBehavior.CloseGap
            onToggled: {
                newDeleteBehaviorRequested(DeleteBehavior.CloseGap)
            }
        }
    }

    RoundedRectangle {
        width: parent.width
        height: gapBehaviorColumn.implicitHeight

        radius: 4
        color: parentBackgroundColor === ui.theme.backgroundPrimaryColor ? ui.theme.backgroundSecondaryColor : ui.theme.backgroundPrimaryColor
        border.color: ui.theme.strokeColor
        border.width: 1
        visible: root.deleteBehavior === DeleteBehavior.CloseGap

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

            RoundedRadioButton {
                checked: root.closeGapBehavior === CloseGapBehavior.ClipRipple
                text: qsTrc("trackedit/preferences", "The selected clip moves back to fill the gap")

                navigation.name: "ClipMovesBackRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    newCloseGapBehaviorRequested(CloseGapBehavior.ClipRipple)
                }
            }

            RoundedRadioButton {
                checked: root.closeGapBehavior === CloseGapBehavior.TrackRipple
                text: qsTrc("trackedit/preferences", "All clips on the same track move back to fill the gap")

                navigation.name: "TrackMovesBackRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    newCloseGapBehaviorRequested(CloseGapBehavior.TrackRipple)
                }
            }

            RoundedRadioButton {
                checked: root.closeGapBehavior === CloseGapBehavior.AllTracksRipple
                text: qsTrc("trackedit/preferences", "All clips on all tracks move back to fill the gap")

                navigation.name: "AllTracksMoveBackRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    newCloseGapBehaviorRequested(CloseGapBehavior.AllTracksRipple)
                }
            }
        }
    }
}
