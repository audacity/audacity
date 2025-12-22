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
        if (!root.settingsModel)
            return qsTrc("trackedit/preferences", "Spectrogram settings")
        else
            return qsTrc("trackedit/preferences", "Spectrogram settings - %1").arg(root.settingsModel.trackTitle)
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

            text: qsTrc("spectrogram/prefs", "Ok")
            buttonRole: ButtonBoxModel.AcceptRole
            buttonId: ButtonBoxModel.Ok
            accentButton: true

            onClicked: {
                settingsModel.accept()
                root.accept()
            }
        }
    }
}
