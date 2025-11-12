/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0
import Audacity.AppShell 1.0

Item {
    id: root

    anchors.fill: parent

    property alias title: titleLabel.text
    property NavigationSection navigationSection: null
    property int navigationStartRow: 2
    property string activeButtonTitle: ""
    property alias leftContent: leftContentItem.data
    property alias rightContent: rightContentItem.data
    property bool showRightContent: true

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ContentPanel"
        enabled: root.enabled && root.visible
        section: root.navigationSection
        order: root.navigationStartRow
        direction: NavigationPanel.Vertical
    }

    function readInfo() {
        accessibleInfo.readInfo()
    }

    function resetFocus() {
        accessibleInfo.resetFocus()
    }

    AccessibleItem {
        id: accessibleInfo

        accessibleParent: root.navigationPanel.accessible
        visualItem: root
        role: MUAccessible.Button

        //: %1 is the page title, %2 is the active button title (e.g. "Next" or "Done")
        name: qsTrc("appshell/gettingstarted", "%1. %2").arg(root.title).arg(root.activeButtonTitle)

        function readInfo() {
            accessibleInfo.ignored = false
            accessibleInfo.focused = true
        }

        function resetFocus() {
            accessibleInfo.ignored = true
            accessibleInfo.focused = false
        }
    }

    RowLayout {
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.bottom: parent.bottom

        spacing: ui.theme.extra.space_0

        // Left side - Controls
        Item {
            Layout.fillWidth: true
            Layout.fillHeight: true
            Layout.alignment: Qt.AlignTop

            Column {
                anchors.fill: parent
                anchors.margins: 24

                spacing: ui.theme.extra.space_24

                StyledTextLabel {
                    id: titleLabel

                    width: parent.width

                    horizontalAlignment: Qt.AlignLeft
                    font: ui.theme.largeBodyBoldFont
                    wrapMode: Text.Wrap
                }

                Item {
                    id: leftContentItem

                    width: parent.width
                    height: parent.height - titleLabel.height - parent.spacing
                }
            }
        }

        // Right side - Preview/Image
        Item {
            id: rightContentItem

            Layout.fillWidth: root.showRightContent
            Layout.fillHeight: true

            visible: root.showRightContent
        }
    }
}
