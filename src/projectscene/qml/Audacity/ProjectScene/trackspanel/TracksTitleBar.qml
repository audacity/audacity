import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property alias navigation: buttonContainer.navigation
    property alias realtimeEffectsNavigation: effectsTitleBar.navigation
    property alias closeEffectsNavigation: effectsTitleBar.navigation

    property int effectsSectionWidth: 0
    property bool showEffectsSection: false

    property int buttonWidth: 97
    property int buttonHeight: 28
    property int buttonRightMargin: 8
    property int textLeftMargin: 12

    signal effectsSectionCloseRequested()
    signal addRequested(type: int)

    Component.onCompleted: {
        if (effectsSectionWidth == 0) {
            console.warn("effectsSectionWidth is not set ; doing some guesswork")
            effectsSectionWidth = 240
        }
    }

    RowLayout {
        id: rowLayout

        anchors.fill: parent

        spacing: 0

        Rectangle {
            id: effectsTitleBar

            property int padding: parent.height / 4
            property NavigationPanel navigation: NavigationPanel {
                name: "RealtimeEffectsSectionPanel"
                enabled: root.enabled && root.visible && root.showEffectsSection
                section: buttonContainer.navigation.section
                direction: NavigationPanel.Vertical
                order: 0

                accessible.name: qsTrc("projectscene", "Realtime effects")
            }

            Layout.preferredWidth: root.effectsSectionWidth
            Layout.preferredHeight: root.height

            color: ui.theme.backgroundPrimaryColor
            border.color: "transparent"
            border.width: padding
            visible: root.showEffectsSection

            StyledTextLabel {
                anchors.fill: parent
                padding: effectsTitleBar.padding

                text: qsTrc("projectscene", "Realtime effects")
                horizontalAlignment: Text.AlignLeft
                verticalAlignment: Text.AlignVCenter
            }

            MouseArea {
                anchors.fill: parent
            }

            Rectangle {
                anchors.right: parent.right
                width: root.height
                height: root.height

                color: effectsTitleBar.color

                FlatButton {
                    id: closeEffectsSectionButton

                    anchors.centerIn: parent
                    width: parent.width - 2 * effectsTitleBar.padding
                    height: parent.height - 2 * effectsTitleBar.padding

                    navigation.name: "CloseEffectsSection"
                    navigation.panel: effectsTitleBar.navigation
                    navigation.order: 0

                    normalColor: ui.theme.backgroundPrimaryColor
                    hoverHitColor: ui.theme.buttonColor
                    icon: IconCode.CLOSE_X_ROUNDED
                    transparent: true

                    onClicked: {
                        root.effectsSectionCloseRequested()
                    }
                }
            }
        }

        SeparatorLine { }

        Rectangle {
            id: buttonContainer

            color: ui.theme.backgroundPrimaryColor

            width: root.verticalPanelDefaultWidth
            Layout.fillWidth: true
            Layout.preferredHeight: root.height

            property NavigationPanel navigation: NavigationPanel {
                name: "AddTrackPanel"
                enabled: root.enabled && root.visible
                order: closeEffectsSectionButton.navigation.order + 1

                accessible.name: qsTrc("projectscene", "Add track")
            }

            StyledTextLabel {
                anchors.left: buttonContainer.left
                anchors.leftMargin: root.textLeftMargin
                anchors.verticalCenter: buttonContainer.verticalCenter

                text: qsTrc("projectscene", "Tracks")
            }

            FlatButton {
                id: addNewTrackBtn

                width: root.buttonWidth
                height: root.buttonHeight

                anchors.right: buttonContainer.right
                anchors.rightMargin: root.buttonRightMargin
                anchors.verticalCenter: buttonContainer.verticalCenter

                navigation.name: "AddTrack"
                navigation.panel: buttonContainer.navigation
                navigation.order: 0

                backgroundRadius: 3
                normalColor: ui.theme.buttonColor

                text: qsTrc("projectscene", "Add track")

                enabled: true

                icon: IconCode.PLUS

                orientation: Qt.Horizontal

                onClicked: {
                    if (addNewTrack.isOpened) {
                        addNewTrack.close()
                    } else {
                        addNewTrack.open()
                    }
                }

                AddNewTrackPopup {
                    id: addNewTrack

                    onCreateTrack: (type) => {
                        root.addRequested(type)
                    }
                }
            }

        }
    }
}
