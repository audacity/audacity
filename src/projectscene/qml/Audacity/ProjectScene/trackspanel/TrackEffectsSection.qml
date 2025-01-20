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

    readonly property int addEffectButtonHeight: 24
    readonly property int addEffectButtonMargin: 12
    readonly property int headerHeight: 40
    readonly property int itemSpacing: 12

    ColumnLayout {
        id: trackEffects
        spacing: 0
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right

        SeparatorLine { }

        RowLayout {
            id: trackEffectsHeader

            spacing: itemSpacing

            Layout.fillWidth: true
            Layout.alignment: Qt.AlignHCenter | Qt.AlignVTop

            FlatButton {
                id: trackEffectsPowerButton

                Layout.alignment: Qt.AlignVCenter | Qt.AlignLeft
                Layout.margins: 8
                Layout.preferredWidth: headerHeight - Layout.margins * 2
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
                Layout.preferredHeight: headerHeight
                Layout.maximumWidth: root.width - trackEffectsPowerButton.width - 2 * itemSpacing
                horizontalAlignment: Text.AlignLeft
            }
        }

        SeparatorLine { }

        Rectangle {
            id: effectListContainer
            visible: root.enabled
            color: ui.theme.backgroundSecondaryColor
            Layout.preferredHeight: !effectList.empty * Math.min(effectList.implicitHeight, root.height - addEffectButtonHeight - 2 * addEffectButtonMargin - headerHeight - 2 /* SeparatorLine */)
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
