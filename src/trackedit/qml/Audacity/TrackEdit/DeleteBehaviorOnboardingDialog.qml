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

StyledDialogView {
    id: root

    contentWidth: column.implicitWidth
    contentHeight: applyButtonRow.y + applyButtonRow.height
    frameless: true

    required property int deleteBehavior
    required property int closeGapBehavior

    Column {
        id: column

        width: deleteBehaviorPanel.width

        padding: ui.theme.extra.space_24
        spacing: ui.theme.extra.space_16

        StyledTextLabel {
            text: deleteBehaviorPanel.title
            font.bold: true
        }

        DeleteBehaviorPanel {
            id: deleteBehaviorPanel

            NavigationPanel {
                id: bodyPanel

                name: "bodyPanel"
                section: root.navigationSection
                row: 0
                column: 0
            }

            navigation: bodyPanel

            parentBackgroundColor: ui.theme.backgroundPrimaryColor
            deleteBehavior: root.deleteBehavior
            closeGapBehavior: root.closeGapBehavior

            onNewDeleteBehaviorRequested: function (newDeleteBehavior) {
                root.deleteBehavior = newDeleteBehavior
            }

            onNewCloseGapBehaviorRequested: function (newCloseGapBehavior) {
                root.closeGapBehavior = newCloseGapBehavior
            }
        }
    }

    SeparatorLine {
        id: separatorLine
        anchors.top: column.bottom
        width: parent.width
    }

    Row {
        id: applyButtonRow

        padding: ui.theme.extra.space_12
        anchors.top: separatorLine.bottom
        anchors.right: parent.right

        FlatButton {

            NavigationPanel {
                id: applyPanel

                name: "applyPanel"
                section: root.navigationSection
                row: 0
                column: 1
            }

            navigation.name: "ApplyButton"
            navigation.panel: applyPanel
            navigation.row: deleteBehaviorPanel.lastNavigationRow + 1

            text: qsTrc("global", "Apply")
            height: 28
            minWidth: 80
            accentButton: true
            onClicked: function () {
                root.ret = {
                    errcode: 0,
                    value: {
                        deleteBehavior: root.deleteBehavior,
                        closeGapBehavior: root.closeGapBehavior
                    }
                }
                root.hide()
            }
        }
    }
}
