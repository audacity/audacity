import QtQuick
import QtQuick.Controls
import QtQuick.Layouts

import Muse.Ui
import Muse.UiComponents

import "qrc:/kddockwidgets/private/quick/qml/" as KDDW

KDDW.TitleBarBase {
    id: root

    property var navigationPanel
    property var navigationOrder
    property var contextMenuModel

    property int effectsSectionWidth: 0
    property bool showEffectsSection: false

    signal effectsSectionCloseRequested()
    signal addRequested(type: int)

    anchors.fill: parent
    heightWhenVisible: implicitHeight

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

            Layout.preferredWidth: root.effectsSectionWidth
            Layout.preferredHeight: root.implicitHeight

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
                width: root.implicitHeight
                height: root.implicitHeight

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

        SeparatorLine { }

        FlatButton {
            id: addNewTrackBtn

            width: root.verticalPanelDefaultWidth
            Layout.fillWidth: true
            Layout.preferredHeight: root.implicitHeight

            accessible.name: qsTrc("projectscene", "Add Track")
            backgroundRadius: 0
            normalColor: ui.theme.backgroundSecondaryColor
            hoverHitColor: ui.theme.buttonColor

            text: qsTrc("projectscene", "Add new track")

            //! TODO AU4
            enabled: true //root.isAddingAvailable

            icon: IconCode.PLUS

            orientation: Qt.Horizontal

            backgroundItem: TracksTitleBarBackground {
                mouseArea: addNewTrackBtn.mouseArea
                color: addNewTrackBtn.normalColor
                baseColor: addNewTrackBtn.normalColor
            }

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

    SeparatorLine {
        anchors.top: rowLayout.bottom
    }
}
