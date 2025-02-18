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

    signal effectsSectionCloseButtonClicked()

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
            visible: root.showEffectsSection
            color: ui.theme.backgroundPrimaryColor
            property int padding: parent.height / 4
            border.color: "transparent"
            border.width: padding

            Layout.preferredWidth: root.effectsSectionWidth
            Layout.preferredHeight: root.implicitHeight

            StyledTextLabel {
                text: qsTrc("projectscene", "Realtime effects")
                anchors.fill: parent
                padding: effectsTitleBar.padding
                horizontalAlignment: Text.AlignLeft
                verticalAlignment: Text.AlignVCenter
            }

            MouseArea {
                anchors.fill: parent
            }

            Rectangle {
                color: effectsTitleBar.color
                width: root.implicitHeight
                height: root.implicitHeight
                anchors.right: parent.right
                FlatButton {
                    transparent: true
                    width: parent.width - 2 * effectsTitleBar.padding
                    height: parent.height - 2 * effectsTitleBar.padding
                    anchors.centerIn: parent
                    normalColor: ui.theme.backgroundPrimaryColor
                    hoverHitColor: ui.theme.buttonColor
                    icon: IconCode.CLOSE_X_ROUNDED
                    onClicked: {
                        root.effectsSectionCloseButtonClicked()
                    }
                }
            }
        }

        SeparatorLine { }

        FlatButton {
            id: gripButton

            Layout.preferredHeight: root.implicitHeight
            Layout.preferredWidth: 28
            backgroundRadius: 0

            visible: true
            normalColor: ui.theme.backgroundSecondaryColor
            hoverHitColor: ui.theme.buttonColor

            mouseArea.objectName: root.objectName + "_gripButton"
            mouseArea.cursorShape: Qt.SizeAllCursor
            // do not accept buttons as FlatButton's mouseArea will override
            // DockTitleBar mouseArea and panel will not be draggable
            mouseArea.acceptedButtons: Qt.NoButton

            transparent: false
            contentItem: StyledIconLabel {
                iconCode: IconCode.TOOLBAR_GRIP
            }

            backgroundItem: TracksTitleBarBackground {
                mouseArea: gripButton.mouseArea
                color: gripButton.normalColor
                baseColor: gripButton.normalColor
            }
        }

        SeparatorLine { }

        FlatButton {
            id: addNewTrackBtn

            width: root.verticalPanelDefaultWidth - gripButton.width
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
