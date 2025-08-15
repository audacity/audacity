/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.TrackEdit 1.0
import Audacity.Preferences 1.0

import "."

StyledDialogView {
    id: root

    width: 360
    contentWidth: width
    contentHeight: okButtonRow.y + okButtonRow.height

    title: qsTrc("trackedit/preferences", "Delete preference")

    DeleteBehaviorOnboardingFollowupDialogModel {
        id: model
    }

    Column {
        id: column

        width: parent.width

        padding: 16
        spacing: 7

        StyledTextLabel {
            id: titleLabel
            width: parent.width - 32
            horizontalAlignment: Text.AlignLeft
            text: qsTrc("trackedit/preferences", "Your delete behaviour has been set")
            wrapMode: Text.WordWrap
            font.bold: true
        }

        StyledTextLabelWithCustomLinkActivatedSignal {
            id: descriptionLabel
            width: parent.width - 32
            text: qsTrc("trackedit/preferences", "You can change this at any time in %1.<br/><br/>" +
                        "There are also a variety of new shortcuts that let you quickly access different delete behaviours. Go to %2 to learn more.").arg("<a href=\"link1\">Preferences</a>").arg("<a href=\"link2\">Shortcuts</a>")
            autoOpenLinks: false
            onLinkActivated: (link) => {
                if (link === "link1") {
                    model.openDeleteBehaviorPreferences()
                } else if (link === "link2") {
                    model.openShortcutsPreferences()
                }
            }
            horizontalAlignment: Text.AlignLeft
            wrapMode: Text.WordWrap
        }
    }

    SeparatorLine {
        id: separatorLine
        anchors.top: column.bottom
        width: parent.width
    }

    Row {
        id: okButtonRow

        padding: 12
        anchors.top: separatorLine.bottom
        anchors.right: parent.right

        FlatButton {
            text: qsTrc("global", "Ok")
            height: 28
            minWidth: 80
            accentButton: true
            onClicked: function() {
                root.hide()
            }
        }
    }
}
