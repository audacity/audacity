/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.TrackEdit 1.0

Column {
    id: root

    required property var editPreferencesModel

    spacing: 16

    Row {
        spacing: 24

        Column {
            spacing: 16

            ClipImageButton {
                width: 196
                height: 88

                radius: 5

                source: "qrc:/resources/Colorful.svg"

                onClicked: {
                    editPreferencesModel.setDeleteBehavior(DeleteBehavior.CloseGap)
                }
            }

            RoundedRadioButton {
                id: closeGapRadioBtn

                checked: editPreferencesModel.deleteBehavior === DeleteBehavior.CloseGap
                text: qsTrc("trackedit/preferences", "Close gap (ripple)")

                navigation.name: "CloseGapRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    editPreferencesModel.setDeleteBehavior(DeleteBehavior.CloseGap)
                }
            }
        }

        Column {
            spacing: 16

            ClipImageButton {
                width: 196
                height: 88

                radius: 5

                source: "qrc:/resources/Colorful.svg"

                onClicked: {
                    editPreferencesModel.setDeleteBehavior(DeleteBehavior.LeaveGap)
                }
            }

            RoundedRadioButton {
                checked: editPreferencesModel.deleteBehavior === DeleteBehavior.LeaveGap
                text: qsTrc("trackedit/preferences", "Leave gap")

                navigation.name: "LeaveGapRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    editPreferencesModel.setDeleteBehavior(DeleteBehavior.LeaveGap)
                }
            }
        }
    }

    RoundedRectangle {
        width: parent.width
        height: gapBehaviorColumn.implicitHeight

        radius: 4
        color: ui.theme.backgroundPrimaryColor
        border.color: ui.theme.strokeColor
        border.width: 1
        visible: closeGapRadioBtn.checked

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
                checked: editPreferencesModel.closeGapBehavior === CloseGapBehavior.ClipRipple
                text: qsTrc("trackedit/preferences", "The selected clip moves back to fill the gap")

                navigation.name: "ClipMovesBackRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    editPreferencesModel.setCloseGapBehavior(CloseGapBehavior.ClipRipple)
                }
            }

            RoundedRadioButton {
                checked: editPreferencesModel.closeGapBehavior === CloseGapBehavior.TrackRipple
                text: qsTrc("trackedit/preferences", "All clips on the same track move back to fill the gap")

                navigation.name: "TrackMovesBackRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    editPreferencesModel.setCloseGapBehavior(CloseGapBehavior.TrackRipple)
                }
            }

            RoundedRadioButton {
                checked: editPreferencesModel.closeGapBehavior === CloseGapBehavior.AllTracksRipple
                text: qsTrc("trackedit/preferences", "All clips on all tracks move back to fill the gap")

                navigation.name: "AllTracksMoveBackRadioBtn"
                navigation.panel: root.navigation
                // navigation.row: 3 What's that?

                onToggled: {
                    editPreferencesModel.setCloseGapBehavior(CloseGapBehavior.AllTracksRipple)
                }
            }
        }
    }
}
