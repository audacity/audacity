/*
 * Audacity: A Digital Audio Editor
 */
import QtQuick
import QtQuick.Layouts
import QtQuick.Controls

import Muse.Ui
import Muse.UiComponents

import Audacity.ProjectScene

Rectangle {
    id: root

    property int selectedTrackIndex: -1
    property alias showEffectsSection: effectList.showEffectsSection

    enabled: effectList.trackName !== ""
    color: ui.theme.backgroundPrimaryColor

    ColumnLayout {
        id: trackEffects
        spacing: 0
        readonly property int itemSpacing: 12
        Layout.fillWidth: true

        SeparatorLine { }

        RowLayout {
            id: trackEffectsHeader
            readonly property int headerHeight: 40

            spacing: trackEffects.itemSpacing

            Layout.fillWidth: true
            Layout.preferredHeight: headerHeight

            FlatButton {
                id: trackEffectsPowerButton

                Layout.alignment: Qt.AlignVCenter | Qt.AlignLeft
                Layout.margins: 8
                Layout.preferredWidth: trackEffectsHeader.headerHeight - Layout.margins * 2
                Layout.preferredHeight: Layout.preferredWidth

                icon: IconCode.BYPASS
                iconFont: ui.theme.toolbarIconsFont

                enabled: root.enabled
                accentButton: effectList.trackEffectsActive

                onClicked: {
                    accentButton = !accentButton
                    effectList.trackEffectsActive = accentButton
                }
            }

            StyledTextLabel {
                id: trackNameLabel
                visible: root.enabled
                text: effectList.trackName
                Layout.fillWidth: true
                Layout.fillHeight: true
                Layout.maximumWidth: root.width - trackEffectsPowerButton.width - trackEffectsHeader.spacing - trackEffects.itemSpacing
                horizontalAlignment: Text.AlignLeft
            }
        }

        SeparatorLine {
            id: trackEffectsBottom
            width: effectsSectionWidth
        }

        Rectangle {
            visible: root.enabled
            color: ui.theme.backgroundSecondaryColor
            Layout.preferredHeight: effectList.preferredHeight == 0 ? 0 : effectList.preferredHeight + (effectList.anchors.topMargin + effectList.anchors.bottomMargin)
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVCenter
            Layout.fillWidth: true
            TrackEffectList {
                id: effectList
                color: "transparent"
                anchors {
                    top: parent.top
                    left: parent.left
                    right: parent.right
                    bottom: parent.bottom
                    leftMargin: 4
                    rightMargin: 12
                    topMargin: 8
                    bottomMargin: 8
                }
                onTrackEffectsActiveChanged: {
                    trackEffectsPowerButton.accentButton = trackEffectsActive
                }
            }
        }

        SeparatorLine {
            visible: root.enabled
        }

        FlatButton {
            Layout.fillWidth: true
            Layout.preferredHeight: 24
            Layout.margins: trackEffects.itemSpacing
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

            text: qsTrc("projectscene", "Add effect")

            RealtimeEffectMenuModel {
                id: menuModel
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
