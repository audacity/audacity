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
    id: effectsPanel

    property int selectedTrackIndex: -1

    color: ui.theme.backgroundPrimaryColor

    onSelectedTrackIndexChanged: {
        const selectedTrackItem = view.itemAtIndex(selectedTrackIndex)
        trackEffects.trackName = selectedTrackItem.item.title
    }

    ColumnLayout {
        id: trackEffects
        spacing: 0
        property alias trackName: trackEffectsHeader.trackName
        readonly property int itemSpacing: 12
        Layout.fillWidth: true

        SeparatorLine { }

        RowLayout {
            id: trackEffectsHeader
            property alias trackName: trackNameLabel.text
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

                accentButton: true

                onClicked: {
                    accentButton = !accentButton
                }
            }

            StyledTextLabel {
                id: trackNameLabel
                Layout.fillWidth: true
                Layout.fillHeight: true
                Layout.maximumWidth: effectsPanel.width - trackEffectsPowerButton.width - trackEffectsHeader.spacing - trackEffects.itemSpacing
                horizontalAlignment: Text.AlignLeft
            }
        }

        SeparatorLine {
            id: trackEffectsBottom
            width: effectsSectionWidth
        }

        Rectangle {
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
            }
        }

        SeparatorLine {
            visible: effectList.visible
        }

        FlatButton {
            Layout.fillWidth: true
            Layout.preferredHeight: 24
            Layout.margins: trackEffects.itemSpacing
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

            text: qsTrc("projectscene", "Add effect")

            RtEffectMenuModel {
                id: menuModel
                trackId: view.itemAtIndex(effectsPanel.selectedTrackIndex).item.trackId
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
