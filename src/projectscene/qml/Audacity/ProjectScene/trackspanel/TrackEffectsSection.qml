/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.Effects
import Audacity.ProjectScene

Rectangle {
    id: root

    property alias isMasterTrack: effectList.isMasterTrack
    property alias minimumHeight: prv.addEffectButtonHeight

    property NavigationSection navigationSection: null

    color: ui.theme.backgroundPrimaryColor

    QtObject {
        id: prv
        property bool enabled: effectList.trackName !== ""
        readonly property int addEffectButtonHeight: 24
        readonly property int addEffectButtonMargin: 12
        readonly property int headerHeight: 40
        readonly property int itemSpacing: 12
    }

    ColumnLayout {
        id: trackEffects
        spacing: 0
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        RowLayout {
            id: trackEffectsHeader

            spacing: prv.itemSpacing
            enabled: prv.enabled

            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

            BypassEffectButton {
                id: trackEffectsPowerButton

                property NavigationPanel navigationPanel: NavigationPanel {
                    name: (isMasterTrack ? "Master" : effectList.trackName) + " effects bypass"
                    enabled: root.enabled && root.visible
                    section: root.navigationSection
                    order: 0
                }

                navigation.panel: trackEffectsPowerButton.navigationPanel

                Layout.alignment: Qt.AlignVCenter | Qt.AlignLeft
                Layout.margins: 8
                Layout.preferredWidth: prv.headerHeight - Layout.margins * 2
                Layout.preferredHeight: Layout.preferredWidth

                accentButton: effectList.trackEffectsActive
                isMasterEffect: root.isMasterTrack

                onClicked: {
                    accentButton = !accentButton
                    effectList.trackEffectsActive = accentButton
                }
            }

            StyledTextLabel {
                id: trackNameLabel
                enabled: prv.enabled
                text: prv.enabled ? effectList.trackName : ""
                Layout.fillWidth: true
                Layout.preferredHeight: prv.headerHeight
                Layout.maximumWidth: root.width - trackEffectsPowerButton.width - 2 * prv.itemSpacing
                horizontalAlignment: Text.AlignLeft
            }
        }

        SeparatorLine {
            id: separator
            enabled: prv.enabled
        }

        Rectangle {
            id: effectListContainer
            visible: prv.enabled
            color: ui.theme.backgroundSecondaryColor
            Layout.preferredHeight: !effectList.empty * Math.min(effectList.implicitHeight, root.height - prv.addEffectButtonHeight - 2 * prv.addEffectButtonMargin - prv.headerHeight - separator.height)
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop
            Layout.fillWidth: true
            TrackEffectList {
                id: effectList
                navigationSection: root.navigationSection
                navigationPanelOrderOffset: 1
                color: "transparent"
                anchors.fill: parent
                anchors.leftMargin: 4
                onTrackEffectsActiveChanged: {
                    trackEffectsPowerButton.accentButton = trackEffectsActive
                }
            }
        }

        SeparatorLine {
            visible: prv.enabled && !effectList.empty
        }

        FlatButton {
            id: addEffectButton
            enabled: prv.enabled
            Layout.fillWidth: true
            Layout.minimumHeight: prv.addEffectButtonHeight
            Layout.maximumHeight: prv.addEffectButtonHeight
            Layout.margins: prv.addEffectButtonMargin
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

            property NavigationPanel navigationPanel: NavigationPanel {
                name: (isMasterTrack ? "Master" : effectList.trackName) + " add effect"
                enabled: root.enabled && root.visible
                section: root.navigationSection
                order: effectList.navigationPanelOrderOffset + effectList.count + 1
            }

            navigation.panel: addEffectButton.navigationPanel

            text: qsTrc("projectscene", "Add effect")

            AddEffectMenuModel {
                id: menuModel
                isMasterTrack: effectList.isMasterTrack
            }

            onClicked: function() {
                menuModel.load()
                effectMenuLoader.toggleOpened(menuModel)
            }

            StyledMenuLoader {
                id: effectMenuLoader

                onHandleMenuItem: function(itemId) {
                    menuModel.handleMenuItem(itemId)
                }
            }
        }

        StyledTextLabel {
            id: infoText
            visible: !isMasterTrack && effectList.empty
            text: {
                {
                    const line1 = qsTrc("projectscene", "Realtime effects are non-destructive and can be changed at any time.")
                    const line2 = qsTrc("projectscene", "<a href=\"https://www.audacityteam.org/realtime-video\">Watch video</a>")
                    return line1 + "<br/><div style='line-height:30px;'><br/></div>" + line2
                }
            }

            Layout.fillWidth: true
            Layout.fillHeight: true
            wrapMode: Text.Wrap
            font: ui.theme.bodyFont
            color: ui.theme.fontPrimaryColor
            leftPadding: 16
            rightPadding: 16
            elide: Text.ElideNone
            verticalAlignment: Text.AlignTop
            horizontalAlignment: Text.AlignLeft
        }
    }
}
