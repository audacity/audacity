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

    enabled: effectList.trackName !== ""
    color: ui.theme.backgroundPrimaryColor

    readonly property int addEffectButtonHeight: 24
    readonly property int addEffectButtonMargin: 12
    readonly property int headerHeight: 40
    readonly property int itemSpacing: 12
    readonly property int minimumHeight: headerHeight + addEffectButtonHeight + 2 * addEffectButtonMargin + separator.height + !effectList.empty * effectList.topMargin

    ColumnLayout {
        id: trackEffects
        spacing: 0
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        RowLayout {
            id: trackEffectsHeader

            spacing: itemSpacing

            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

            BypassEffectButton {
                id: trackEffectsPowerButton

                Layout.alignment: Qt.AlignVCenter | Qt.AlignLeft
                Layout.margins: 8
                Layout.preferredWidth: headerHeight - Layout.margins * 2
                Layout.preferredHeight: Layout.preferredWidth

                enabled: root.enabled
                accentButton: effectList.trackEffectsActive
                isMasterEffect: root.isMasterTrack

                onClicked: {
                    accentButton = !accentButton
                    effectList.trackEffectsActive = accentButton
                }
            }

            StyledTextLabel {
                id: trackNameLabel
                text: root.enabled ? effectList.trackName : ""
                Layout.fillWidth: true
                Layout.preferredHeight: headerHeight
                Layout.maximumWidth: root.width - trackEffectsPowerButton.width - 2 * itemSpacing
                horizontalAlignment: Text.AlignLeft
            }
        }

        SeparatorLine {
            id: separator
        }

        Rectangle {
            id: effectListContainer
            visible: root.enabled
            color: ui.theme.backgroundSecondaryColor
            Layout.preferredHeight: !effectList.empty * Math.min(effectList.implicitHeight, root.height - addEffectButtonHeight - 2 * addEffectButtonMargin - headerHeight - separator.height)
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop
            Layout.fillWidth: true
            TrackEffectList {
                id: effectList
                color: "transparent"
                anchors.fill: parent
                anchors.leftMargin: 4
                onTrackEffectsActiveChanged: {
                    trackEffectsPowerButton.accentButton = trackEffectsActive
                }
            }
        }

        SeparatorLine {
            visible: root.enabled && !effectList.empty
        }

        FlatButton {
            id: addEffectButton
            Layout.fillWidth: true
            Layout.minimumHeight: addEffectButtonHeight
            Layout.maximumHeight: addEffectButtonHeight
            Layout.margins: addEffectButtonMargin
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

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
    }
}
