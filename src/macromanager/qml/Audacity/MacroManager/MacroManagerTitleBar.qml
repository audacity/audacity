/*
* Audacity: A Digital Audio Editor
*/
pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Rectangle {
    id: root

    property string title: ""

    property NavigationPanel navigationPanel: null
    property int navigationOrder: 0
    property var contextMenuModel: null

    implicitHeight: content.implicitHeight

    color: ui.theme.backgroundTertiaryColor

    QtObject {
        id: prv

        readonly property int sideMargin: 8
        readonly property int verticalMargin: 2
        readonly property int spacing: 8
        readonly property int menuButtonSize: 20
    }

    Rectangle {
        id: content

        anchors.left: parent.left
        anchors.top: parent.top
        anchors.bottom: parent.bottom

        width: row.implicitWidth + 2 * prv.sideMargin
        implicitHeight: row.implicitHeight + 2 * prv.verticalMargin

        color: ui.theme.backgroundPrimaryColor

        RowLayout {
            id: row

            anchors.fill: parent
            anchors.leftMargin: prv.sideMargin
            anchors.rightMargin: prv.sideMargin

            spacing: prv.spacing

            StyledTextLabel {
                text: root.title
                horizontalAlignment: Qt.AlignLeft
            }

            MenuButton {
                Layout.preferredWidth: prv.menuButtonSize
                Layout.preferredHeight: prv.menuButtonSize

                navigation.panel: root.navigationPanel
                navigation.order: root.navigationOrder
                menuModel: root.contextMenuModel

                onHandleMenuItem: function (itemId) {
                    root.contextMenuModel.handleMenuItem(itemId)
                }
            }
        }
    }
}
