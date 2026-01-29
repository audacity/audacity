
/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick 2.15
import QtQuick.Layouts 1.15

import Muse.Ui 1.0
import Muse.UiComponents
import Audacity.AppShell

Item {
    id: root

    property string activeButtonTitle: ""
    property int navigationStartRow: 2
    property NavigationSection navigationSection: null
    property NavigationPanel navigationPanel: NavigationPanel {
        name: "ContentPanel"
        enabled: root.enabled && root.visible
        section: root.navigationSection
        order: root.navigationStartRow
        direction: NavigationPanel.Vertical
    }

    QtObject {
        id: prv

        readonly property int columnSpacing: 24
        readonly property int titleTextSpacing: 16
        readonly property int contentMargins: 24
        readonly property int contentTextSpacing: 8

        readonly property string preferencesLink: "<a href=\"preferences\">%1</a>".arg(qsTrc("appshell/gettingstarted", "Preferences"))
        readonly property string preferencesUrl: "audacity://preferences"
        readonly property string updatesTitleText: qsTrc("appshell/gettingstarted", "App Updates")
        readonly property string updatesBodyText1: qsTrc("appshell/gettingstarted", 
                                                        "Audacity notifies you in-app when a new version is available to download.")
        readonly property string updatesBodyText2: qsTrc("appshell/gettingstarted", "You can turn this off anytime in %1.")
                                                     .arg(prv.preferencesLink)
        readonly property string usageInfoTitleText: qsTrc("appshell/gettingstarted", "Usage Info")
        readonly property string usageInfoBodyText1: qsTrc("appshell/gettingstarted", 
                                                        "To help us understand how often people use Audacity, we generate a random ID (UUID) for each installation. This ID does not contain any personally identifiable information.")
        readonly property string usageInfoBodyText2: qsTrc("appshell/gettingstarted", "You can disable this anytime in %1.")
                                                     .arg(prv.preferencesLink)
        readonly property string enableUUIDText: qsTrc("appshell/gettingstarted", "Enable UUID")
        readonly property string privacyPolicyUrl: "https://www.audacityteam.org/legal/privacy-notice/"
        readonly property string privacyPolicyLink: "<a href=\"%1\">%2</a>".arg(prv.privacyPolicyUrl).arg(qsTrc("appshell/gettingstarted", "Privacy Policy"))
        readonly property string privacyPolicyInfoText: qsTrc("appshell/gettingstarted", 
                                                        "Want to know more? Check out our %1")
                                                        .arg(prv.privacyPolicyLink)
    }

    AppUpdateUsageInfoPageModel {
        id: model
    }

    Component.onDestruction: {
        model.setSendAnonymousUsageInfo(enableUUIDCheckBox.checked)
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
        name: qsTrc("appshell/gettingstarted", "UpdatesUsage. %1").arg(root.activeButtonTitle)

        function readInfo() {
            accessibleInfo.ignored = false
            accessibleInfo.focused = true
        }

        function resetFocus() {
            accessibleInfo.ignored = true
            accessibleInfo.focused = false
        }
    }

    Column {
        id: content

        anchors.fill: parent
        anchors.margins: prv.contentMargins
        spacing: prv.columnSpacing

        Column {
            id: updatesColumn

            width: parent.width

            spacing: prv.titleTextSpacing

            StyledTextLabel {
                text: prv.updatesTitleText
                font: ui.theme.largeBodyBoldFont
            }

            Column {
                width: parent.width
                spacing: prv.contentTextSpacing

                StyledTextLabel {
                    width: parent.width
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.Wrap

                    text: prv.updatesBodyText1
                }

                Text {
                    width: parent.width
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.Wrap

                    color: ui.theme.fontPrimaryColor
                    linkColor: ui.theme.linkColor
                    font: ui.theme.bodyFont

                    text: prv.updatesBodyText2

                    onLinkActivated: function(link) {
                        api.launcher.open(prv.preferencesUrl)
                    }

                    MouseArea {
                        anchors.fill: parent
                        acceptedButtons: Qt.NoButton
                        cursorShape: parent.hoveredLink ? Qt.PointingHandCursor : Qt.ArrowCursor
                    }
                }
            }
        }

        Column {
            id: usageInfoColumn

            width: parent.width

            spacing: prv.titleTextSpacing

            StyledTextLabel {
                text: prv.usageInfoTitleText
                font: ui.theme.largeBodyBoldFont
            }

            Column {
                width: parent.width
                spacing: prv.contentTextSpacing

                StyledTextLabel {
                    width: parent.width
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.Wrap

                    text: prv.usageInfoBodyText1
                }

                Text {
                    width: parent.width
                    horizontalAlignment: Text.AlignLeft
                    wrapMode: Text.Wrap

                    color: ui.theme.fontPrimaryColor
                    linkColor: ui.theme.linkColor
                    font: ui.theme.bodyFont

                    text: prv.usageInfoBodyText2

                    onLinkActivated: function(link) {
                        api.launcher.open(prv.preferencesUrl)
                    }

                    MouseArea {
                        anchors.fill: parent
                        acceptedButtons: Qt.NoButton
                        cursorShape: parent.hoveredLink ? Qt.PointingHandCursor : Qt.ArrowCursor
                    }
                }
            }

            CheckBox {
                id: enableUUIDCheckBox

                navigation.name: "EnableUUIDCheckbox"
                navigation.panel: root.navigationPanel
                navigation.row: root.navigationStartRow + 1
                navigation.column: 0

                text: prv.enableUUIDText
                checked: true

                onClicked: {
                    checked = !checked
                }
            }

            StyledTextLabel {
                width: parent.width
                horizontalAlignment: Text.AlignLeft
                wrapMode: Text.Wrap

                text: prv.privacyPolicyInfoText
            }
        }
    }
}