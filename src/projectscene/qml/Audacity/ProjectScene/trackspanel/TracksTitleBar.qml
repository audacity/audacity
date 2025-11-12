import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

Item {
    id: root

    property alias navigation: buttonContainer.navigation

    property int effectsSectionWidth: 0
    property bool showEffectsSection: false

    property int buttonWidth: 97
    property int buttonHeight: 28
    property int buttonRightMargin: ui.theme.extra.space_8
    property int textLeftMargin: ui.theme.extra.space_12

    signal effectsSectionCloseRequested
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

        spacing: ui.theme.extra.space_0

        Rectangle {
            id: effectsTitleBar

            property int padding: parent.height / 4

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
                    anchors.centerIn: parent
                    width: parent.width - 2 * effectsTitleBar.padding
                    height: parent.height - 2 * effectsTitleBar.padding

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

        SeparatorLine {}

        Rectangle {
            id: buttonContainer

            color: ui.theme.backgroundPrimaryColor

            width: root.verticalPanelDefaultWidth
            Layout.fillWidth: true
            Layout.preferredHeight: root.height

            property NavigationPanel navigation: NavigationPanel {
                name: "AddTrackPanel"
                enabled: root.enabled && root.visible
                order: 1

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

                    onCreateTrack: type => {
                        root.addRequested(type)
                    }
                }
            }
        }
    }
}
