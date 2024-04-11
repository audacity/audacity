/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.ProjectScene 1.0

import "internal"

Item {
    id: root

    property alias orientation: gridView.orientation

    property bool floating: false

    property int maximumWidth: 0
    property int maximumHeight: 0

    width: gridView.isHorizontal ? childrenRect.width : 76
    height: !gridView.isHorizontal ? childrenRect.height : 40

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "PlaybackToolBar"
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("projectscene", "Playback toolbar")
    }

    PlaybackToolBarModel {
        id: toolbarModel
    }

    QtObject {
        id: prv

        function resolveHorizontalGridViewWidth() {
            if (root.floating) {
                return gridView.contentWidth
            }

            var requiredFreeSpace = gridView.cellWidth * 3 + gridView.rowSpacing * 4

            if (root.maximumWidth - gridView.contentWidth < requiredFreeSpace) {
                return gridView.contentWidth - requiredFreeSpace
            }

            return gridView.contentWidth
        }

        function resolveVerticalGridViewHeight() {
            if (root.floating) {
                return gridView.contentHeight
            }

            var requiredFreeSpace = gridView.cellHeight * 3 + gridView.rowSpacing * 4

            if (root.maximumHeight - gridView.contentHeight < requiredFreeSpace) {
                return gridView.contentHeight - requiredFreeSpace
            }

            return gridView.contentHeight
        }
    }

    Component.onCompleted: {
        toolbarModel.load()
    }

    GridViewSectional {
        id: gridView

        sectionRole: "section"

        rowSpacing: 4
        columnSpacing: 4

        cellWidth: 32
        cellHeight: cellWidth

        clip: true

        model: toolbarModel

        sectionDelegate: SeparatorLine {
            orientation: gridView.orientation
            visible: itemIndex !== 0
        }

        itemDelegate: FlatButton {
            id: btn

            property var item: Boolean(itemModel) ? itemModel.itemRole : null
            property var hasMenu: Boolean(item) && item.subitems.length !== 0

            width: gridView.cellWidth
            height: gridView.cellWidth

            accentButton: (Boolean(item) && item.checked) || menuLoader.isMenuOpened
            transparent: !accentButton

            icon: Boolean(item) ? item.icon : IconCode.NONE
            iconFont: ui.theme.toolbarIconsFont

            toolTipTitle: Boolean(item) ? item.title : ""
            toolTipDescription: Boolean(item) ? item.description : ""
            toolTipShortcut: Boolean(item) ? item.shortcuts : ""

            navigation.panel: root.navigationPanel
            navigation.name: Boolean(item) ? item.id : ""
            navigation.order: Boolean(itemModel) ? itemModel.order : 0
            isClickOnKeyNavTriggered: false
            navigation.onTriggered: {
                if (menuLoader.isMenuOpened || hasMenu) {
                    toggleMenuOpened()
                } else {
                    handleMenuItem()
                }
            }

            mouseArea.acceptedButtons: hasMenu && itemModel.isMenuSecondary
                                       ? Qt.LeftButton | Qt.RightButton
                                       : Qt.LeftButton

            function toggleMenuOpened() {
                menuLoader.toggleOpened(item.subitems)
            }

            function handleMenuItem() {
                Qt.callLater(toolbarModel.handleMenuItem, item.id)
            }

            onClicked: function(mouse) {
                if (menuLoader.isMenuOpened // If already menu open, close it
                        || (hasMenu // Or if can open menu
                            && (!itemModel.isMenuSecondary // And _should_ open menu
                                || mouse.button === Qt.RightButton))) {
                    toggleMenuOpened()
                    return
                }

                if (mouse.button === Qt.LeftButton) {
                    handleMenuItem()
                }
            }

            Connections {
                target: btn.mouseArea

                // Make sure we only connect to `pressAndHold` if necessary
                // See https://github.com/musescore/MuseScore/issues/16012
                enabled: btn.hasMenu && !menuLoader.isMenuOpened

                function onPressAndHold() {
                    if (menuLoader.isMenuOpened || !btn.hasMenu) {
                        return
                    }

                    btn.toggleMenuOpened()
                }
            }

            Canvas {
                visible: Boolean(itemModel) && itemModel.isMenuSecondary

                property color fillColor: ui.theme.fontPrimaryColor
                onFillColorChanged: {
                    requestPaint()
                }

                width: 4
                height: 4

                anchors.margins: 2
                anchors.right: parent.right
                anchors.bottom: parent.bottom

                onPaint: {
                    const ctx = getContext("2d");
                    ctx.fillStyle = fillColor;
                    ctx.moveTo(width, 0);
                    ctx.lineTo(width, height);
                    ctx.lineTo(0, height);
                    ctx.closePath();
                    ctx.fill();
                }
            }

            StyledMenuLoader {
                id: menuLoader

                onHandleMenuItem: function(itemId) {
                    toolbarModel.handleMenuItem(itemId)
                }
            }
        }
    }

    FlatButton {
        id: customizeButton

        anchors.margins: 4

        width: gridView.cellWidth
        height: gridView.cellHeight

        icon: IconCode.SETTINGS_COG
        iconFont: ui.theme.toolbarIconsFont
        toolTipTitle: qsTrc("projectscene", "Customize toolbar")
        toolTipDescription: qsTrc("projectscene", "Show/hide toolbar buttons")
        transparent: true

        navigation.panel: root.navigationPanel
        navigation.order: 100
        navigation.accessible.name: qsTrc("projectscene", "Customize toolbar")

        onClicked: {
            customizePopup.toggleOpened()
        }

        PlaybackToolBarCustomisePopup {
            id: customizePopup

            anchorItem: !root.floating ? ui.rootItem : null
        }
    }

    states: [
        State {
            when: gridView.isHorizontal

            PropertyChanges {
                target: gridView
                width: prv.resolveHorizontalGridViewWidth()
                height: root.height
                sectionWidth: 1
                sectionHeight: root.height
                rows: 1
                columns: gridView.noLimit
            }

            AnchorChanges {
                target: customizeButton
                anchors.left: gridView.right
                anchors.verticalCenter: root.verticalCenter
            }
        },
        State {
            when: !gridView.isHorizontal

            PropertyChanges {
                target: gridView
                width: root.width
                height: prv.resolveVerticalGridViewHeight()
                sectionWidth: root.width
                sectionHeight: 1
                rows: gridView.noLimit
                columns: 2
            }

            AnchorChanges {
                target: customizeButton
                anchors.top: gridView.bottom
                anchors.right: parent.right
            }
        }
    ]
}
