/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import Muse.UiComponents
import Audacity.TrackEdit
import Preferences

import "."

StyledDialogView {
    id: root

    contentWidth: mainColumn.width
    contentHeight: mainColumn.height + separator.height + bbox.height

    title: {
        let title = qsTrc("trackedit/preferences", "Spectrogram settings")
        if (settingsModel.trackTitle !== "")
            title += " - " + settingsModel.trackTitle
        return title
    }

    property int trackId: -1

    property var settingsModel: TrackSpectrogramSettingsModel {
        trackId: root.trackId
    }

    QtObject {
        id: prv

        readonly property int buttonHeight: 28
    }

    Column {
        id: mainColumn

        width: 480

        spacing: 16
        padding: 16

        TrackSpectrogramSelectionSection {
            width: parent.width
            settingsModel: root.settingsModel
            onActiveFocusRequested: function (rect) {
                root.ensureContentVisibleRequested(rect)
            }
        }

        SeparatorLine {}

        TrackSpectrogramScaleSection {
            width: parent.width
            settingsModel: root.settingsModel
            onActiveFocusRequested: function (rect) {
                root.ensureContentVisibleRequested(rect)
            }
        }

        SeparatorLine {}

        TrackSpectrogramColorsSection {
            width: parent.width
            settingsModel: root.settingsModel
            onActiveFocusRequested: function (rect) {
                root.ensureContentVisibleRequested(rect)
            }
        }

        SeparatorLine {}

        TrackSpectrogramAlgorithmSection {
            width: parent.width
            settingsModel: root.settingsModel
            onActiveFocusRequested: function (rect) {
                root.ensureContentVisibleRequested(rect)
            }
        }
    }

    SeparatorLine {
        id: separator
        anchors.top: mainColumn.bottom
    }

    ButtonBox {
        id: bbox

        anchors.top: separator.bottom
        width: parent.width

        padding: 12
        spacing: 8

        FlatButton {
            id: previewBtn

            height: prv.buttonHeight
            minWidth: 80
            isLeftSide: true

            text: qsTrc("spectrogram/prefs", "Preview")
            buttonRole: ButtonBoxModel.CustomRole
            buttonId: ButtonBoxModel.CustomButton + 1

            onClicked: settingsModel.preview()
        }

        FlatButton {
            id: cancelBtn

            height: prv.buttonHeight
            minWidth: 80

            text: qsTrc("spectrogram/prefs", "Cancel")
            buttonRole: ButtonBoxModel.RejectRole
            buttonId: ButtonBoxModel.Cancel

            onClicked: {
                root.reject()
            }
        }

        FlatButton {
            id: okBtn

            height: prv.buttonHeight
            minWidth: 80

            text: qsTrc("spectrogram/prefs", "Apply")
            buttonRole: ButtonBoxModel.AcceptRole
            buttonId: ButtonBoxModel.Apply
            accentButton: true

            onClicked: {
                settingsModel.apply()
                root.accept()
            }
        }
    }
}
