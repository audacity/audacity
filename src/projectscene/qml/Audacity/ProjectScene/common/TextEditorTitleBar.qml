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

    signal newFile()
    signal saveFile()
    signal openFile()

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

            backgroundItem: TextEditorTitleBarBackground {
                mouseArea: gripButton.mouseArea
                color: gripButton.normalColor
                baseColor: gripButton.normalColor
            }
        }

        SeparatorLine { }

        FlatButton {
            id: newFileBtn

            width: (root.verticalPanelDefaultWidth - gripButton.width) / 3
            Layout.fillWidth: true
            Layout.preferredHeight: root.implicitHeight

            accessible.name: qsTrc("projectscene", "New file")
            backgroundRadius: 0
            normalColor: ui.theme.backgroundSecondaryColor
            hoverHitColor: ui.theme.buttonColor

            text: qsTrc("projectscene", "New file")

            //! TODO AU4
            enabled: true //root.isAddingAvailable

            icon: IconCode.NEW_FILE

            orientation: Qt.Horizontal

            backgroundItem: TextEditorTitleBarBackground {
                mouseArea: newFileBtn.mouseArea
                color: newFileBtn.normalColor
                baseColor: newFileBtn.normalColor
            }

            onClicked: {
                root.newFile()
            }
        }

        SeparatorLine { }

        FlatButton {
            id: openBtn

            width: (root.verticalPanelDefaultWidth - gripButton.width) / 3
            Layout.fillWidth: true
            Layout.preferredHeight: root.implicitHeight

            accessible.name: qsTrc("projectscene", "Open notes")
            backgroundRadius: 0
            normalColor: ui.theme.backgroundSecondaryColor
            hoverHitColor: ui.theme.buttonColor

            text: qsTrc("projectscene", "Open notes")

            //! TODO AU4
            enabled: true //root.isAddingAvailable

            icon: IconCode.OPEN_FILE

            orientation: Qt.Horizontal

            backgroundItem: TextEditorTitleBarBackground {
                mouseArea: openBtn.mouseArea
                color: openBtn.normalColor
                baseColor: openBtn.normalColor
            }

            onClicked: {
                root.openFile()
            }
        }

        SeparatorLine { }

        FlatButton {
            id: saveBtn

            width: (root.verticalPanelDefaultWidth - gripButton.width) / 3
            Layout.fillWidth: true
            Layout.preferredHeight: root.implicitHeight

            accessible.name: qsTrc("projectscene", "Save notes")
            backgroundRadius: 0
            normalColor: ui.theme.backgroundSecondaryColor
            hoverHitColor: ui.theme.buttonColor

            text: qsTrc("projectscene", "Save notes")

            //! TODO AU4
            enabled: true //root.isAddingAvailable

            icon: IconCode.SAVE

            orientation: Qt.Horizontal

            backgroundItem: TextEditorTitleBarBackground {
                mouseArea: saveBtn.mouseArea
                color: saveBtn.normalColor
                baseColor: saveBtn.normalColor
            }

            onClicked: {
                root.saveFile()
            }
        }
    }

    SeparatorLine {
        anchors.top: rowLayout.bottom
    }
}
