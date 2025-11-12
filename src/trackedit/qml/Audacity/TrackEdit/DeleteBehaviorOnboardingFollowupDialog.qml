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

    NavigationPanel {
        id: navigationPanel

        section: root.navigationSection
    }

    Column {
        id: column

        width: parent.width

        padding: 16
        spacing: ui.theme.extra.space_7

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
            horizontalAlignment: Text.AlignLeft

            text: {
                const editingPrefsLink = '<a href="editing-preferences">%1</a>'.arg(qsTrc("trackedit/preferences", "Preferences"))
                const line1 = qsTrc("trackedit/preferences", "You can change this at any time in %1.").arg(editingPrefsLink)

                const shortcutsPrefsLink = '<a href="shortcuts-preferences">%1</a>'.arg(qsTrc("trackedit/preferences", "Shortcuts"))
                const line2 = qsTrc("trackedit/preferences", "There are also a variety of new shortcuts that let you quickly access different delete behaviours. Go to %2 to learn more.").arg(shortcutsPrefsLink)

                return line1 + "<br/><br/>" + line2
            }
            autoOpenLinks: false
            onLinkActivated: action => {
                root.ret = {
                    errcode: 0,
                    value: action
                }
                root.hide()
            }
            wrapMode: Text.WordWrap
        }
    }

    SeparatorLine {
        id: separatorLine

        width: parent.width

        anchors.top: column.bottom
    }

    Row {
        id: okButtonRow

        anchors.top: separatorLine.bottom
        anchors.right: parent.right

        padding: 12

        FlatButton {
            height: 28
            minWidth: 80

            navigation.name: "OkButton"
            navigation.panel: navigationPanel

            text: qsTrc("global", "Ok")
            accentButton: true
            onClicked: function () {
                root.hide()
            }
        }
    }
}
