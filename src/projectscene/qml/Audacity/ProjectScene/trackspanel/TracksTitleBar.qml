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

    property int expectedHeight: 39

    signal addRequested(type: int)

    anchors.fill: parent
    implicitHeight: gripButton.implicitHeight
    heightWhenVisible: expectedHeight

    RowLayout {
        id: rowLayout
        anchors.fill: parent
        spacing: 0

        FlatButton {
            id: gripButton

            Layout.preferredHeight: root.expectedHeight
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
            Layout.preferredHeight: root.expectedHeight

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
