/*
* Audacity: A Digital Audio Editor
*/
import QtQuick 2.15

import Muse.Ui 1.0
import Muse.UiComponents 1.0

import Audacity.Playback 1.0

import "internal"

Item {
    id: root

    property bool floating: false

    property int maximumWidth: 0
    property int maximumHeight: 0

    width: gridView.width + /*spacing*/ 4 + customizeButton.width
    height: gridView.height

    property NavigationPanel navigationPanel: NavigationPanel {
        name: "PlaybackToolBar"
        enabled: root.enabled && root.visible
        accessible.name: qsTrc("playback", "Playback toolbar")
    }

    PlaybackToolBarModel {
        id: toolbarModel
    }

    Component.onCompleted: {
        toolbarModel.load()
    }

    Flow {
        id: gridView

        clip: true

        spacing: 4

        Repeater {
            model: toolbarModel

            Loader {
                id: loader

                property var itemData: Boolean(model) ? model.item : null
                property var itemOrder: Boolean(model) ? model.order : 0
                property var itemIsMenuSecondary: Boolean(model) ? model.isMenuSecondary : false

                property var itemSize: {
                    if (!Boolean(loader.itemData)) {
                        return null
                    }

                    switch(loader.itemData.type) {
                    case PlaybackToolBarItem.SECTION: return Qt.size(1, 32)
                    case PlaybackToolBarItem.ACTION: return Qt.size(32, 32)
                    case PlaybackToolBarItem.PLAYBACK_LEVEL: return Qt.size(128, 32)
                    }

                    return null
                }

                width: Boolean(item) ? item.width : 0
                height: 32

                sourceComponent: {
                    if (!Boolean(loader.itemData)) {
                        return null
                    }

                    switch(loader.itemData.type) {
                    case PlaybackToolBarItem.SECTION: return sectionComp
                    case PlaybackToolBarItem.ACTION: return actionComp
                    case PlaybackToolBarItem.PLAYBACK_LEVEL: return playbackLevelComp
                    }

                    return null
                }

                Component {
                    id: sectionComp

                    SeparatorLine {
                        orientation: Qt.Vertical
                    }
                }

                Component {
                    id: actionComp

                    FlatButton {
                        id: btn

                        property var item: loader.itemData
                        property int order: loader.itemOrder
                        property var isMenuSecondary: loader.itemIsMenuSecondary

                        property var hasMenu: Boolean(item) && item.subitems.length !== 0

                        width: 32
                        height: width

                        accentButton: item.checked || menuLoader.isMenuOpened
                        transparent: !accentButton

                        icon: item.icon
                        iconFont: ui.theme.toolbarIconsFont

                        toolTipTitle: item.title
                        toolTipDescription: item.description
                        toolTipShortcut: item.shortcuts

                        navigation.panel: root.navigationPanel
                        navigation.name: item.id
                        navigation.order: order
                        isClickOnKeyNavTriggered: false
                        navigation.onTriggered: {
                            if (menuLoader.isMenuOpened || hasMenu) {
                                toggleMenuOpened()
                            } else {
                                handleMenuItem()
                            }
                        }

                        mouseArea.acceptedButtons: hasMenu && isMenuSecondary
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
                                        && (!isMenuSecondary // And _should_ open menu
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

                            enabled: btn.hasMenu && !menuLoader.isMenuOpened

                            function onPressAndHold() {
                                if (menuLoader.isMenuOpened || !btn.hasMenu) {
                                    return
                                }

                                btn.toggleMenuOpened()
                            }
                        }

                        Canvas {
                            visible: isMenuSecondary

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

                Component {
                    id: playbackLevelComp

                    PlaybackLevel {
                        property var item: loader.itemData

                        width: 128

                        onVolumeLevelChangeRequested: function(level) {
                            item.level = level
                        }
                    }
                }
            }
        }

    }

    FlatButton {
        id: customizeButton

        anchors.margins: 4
        anchors.left: gridView.right
        anchors.verticalCenter: root.verticalCenter

        width: 32
        height: width

        icon: IconCode.SETTINGS_COG
        iconFont: ui.theme.toolbarIconsFont
        toolTipTitle: qsTrc("playback", "Customize toolbar")
        toolTipDescription: qsTrc("playback", "Show/hide toolbar buttons")
        transparent: true

        navigation.panel: root.navigationPanel
        navigation.order: 100
        navigation.accessible.name: qsTrc("playback", "Customize toolbar")

        onClicked: {
            customizePopup.toggleOpened()
        }

        PlaybackToolBarCustomisePopup {
            id: customizePopup

            anchorItem: !root.floating ? ui.rootItem : null
        }
    }
}
