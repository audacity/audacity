/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.9
import QtQuick.Controls 2.9
import QtQuick.Layouts 1.9

import com.kdab.dockwidgets 1.0

Rectangle {
    id: root

    property QtObject frameCpp
    readonly property QtObject titleBarCpp: frameCpp ? frameCpp.titleBar : null
    readonly property int nonContentsHeight: (titleBar.item ? titleBar.item.heightWhenVisible : 0) + tabbar.implicitHeight + (2 * contentsMargin) + titleBarContentsMargin
    property int contentsMargin: isMDI ? 2 : 1
    property int titleBarContentsMargin: 1
    property bool hasCustomMouseEventRedirector: false
    property int mouseResizeMargin: 8
    readonly property bool isMDI: frameCpp && frameCpp.isMDI
    readonly property bool resizeAllowed: root.isMDI && !_kddwDragController.isDragging && _kddwDockRegistry && (!_kddwDockRegistry.frameInMDIResize || _kddwDockRegistry.frameInMDIResize === frameCpp)
    property alias tabBarHeight: tabbar.height

    anchors.fill: parent

    radius: 2
    color: "transparent"
    border {
        color: "#b8b8b8"
        width: 1
    }

    onFrameCppChanged: {
        if (frameCpp) {
            frameCpp.setStackLayout(stackLayout);
        }
    }

    onNonContentsHeightChanged: {
        if (frameCpp)
            frameCpp.geometryUpdated();
    }

    ResizeHandlerHelper {
        anchors {
            left: parent ? parent.left : undefined
            top: parent ? parent.top : undefined
            bottom: parent ? parent.bottom : undefined
        }

        width: resizeMargin
        z: 100
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Left
    }

    ResizeHandlerHelper {
        anchors {
            right: parent ? parent.right : undefined
            top: parent ? parent.top : undefined
            bottom: parent ? parent.bottom : undefined
        }

        width: resizeMargin
        z: 100
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Right
    }

    ResizeHandlerHelper {
        anchors {
            right: parent ? parent.right : undefined
            top: parent ? parent.top : undefined
            left: parent ? parent.left : undefined
        }

        height: resizeMargin
        z: 100
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Top
    }

    ResizeHandlerHelper {
        anchors {
            right: parent ? parent.right : undefined
            left: parent ? parent.left : undefined
            bottom: parent ?  parent.bottom : undefined
        }

        height: resizeMargin
        z: 100
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Bottom
    }

    ResizeHandlerHelper {
        anchors {
            right: parent ? parent.right : undefined
            bottom: parent ? parent.bottom : undefined
        }

        height: width
        width: resizeMargin
        z: 101
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Bottom | KDDockWidgets.CursorPosition_Right
    }

    ResizeHandlerHelper {
        anchors {
            left:  parent ? parent.left : undefined
            top:  parent ? parent.top : undefined
        }

        height: width
        width: resizeMargin
        z: 101
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Top | KDDockWidgets.CursorPosition_Left
    }

    ResizeHandlerHelper {
        anchors {
            right: parent ? parent.right : undefined
            top: parent ? parent.top : undefined
        }

        height: width
        width: resizeMargin
        z: 101
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Top | KDDockWidgets.CursorPosition_Right
    }

    ResizeHandlerHelper {
        anchors {
            left: parent ? parent.left : undefined
            bottom: parent ? parent.bottom : undefined
        }

        height: width
        width: resizeMargin
        z: 101
        frameCpp: root.frameCpp
        resizeAllowed: root.resizeAllowed
        resizeMargin: root.mouseResizeMargin
        cursorPosition: KDDockWidgets.CursorPosition_Left | KDDockWidgets.CursorPosition_Bottom
    }

    Loader {
        id: titleBar
        readonly property QtObject titleBarCpp: root.titleBarCpp
        source: frameCpp ? _kddw_widgetFactory.titleBarFilename()
                         : ""

        anchors {
            top:  parent ? parent.top : undefined
            left: parent ? parent.left : undefined
            right: parent ? parent.right : undefined
            topMargin: root.titleBarContentsMargin
            leftMargin: root.titleBarContentsMargin
            rightMargin: root.titleBarContentsMargin
        }
    }

    Connections {
        target: frameCpp
        function onCurrentIndexChanged() {
            tabbar.currentIndex = frameCpp.currentIndex;
        }
    }

    MouseArea {
        id: dragMouseArea
        objectName: "kddwTabBarDragMouseArea"
        hoverEnabled: true
        anchors.fill: tabbar
        enabled: tabbar.visible
        z: 10
    }

    TabBar {
        id: tabbar

        readonly property QtObject tabBarCpp: root.frameCpp ? root.frameCpp.tabWidget.tabBar
                                                            : null

        visible: count > 1
        height: visible ? implicitHeight : 0

        anchors {
            left: parent ? parent.left : undefined
            right: parent ? parent.right : undefined
            top: (titleBar && titleBar.visible) ? titleBar.bottom
                                                : (parent ? parent.top : undefined)
            topMargin: 1
            leftMargin: 1
            rightMargin: 1
        }

        width: parent ? parent.width : 0

        onCurrentIndexChanged: {
            if (root && root.frameCpp)
                root.frameCpp.tabWidget.setCurrentDockWidget(currentIndex);
        }

        onTabBarCppChanged: {
            if (tabBarCpp) {
                if (!root.hasCustomMouseEventRedirector)
                    tabBarCpp.redirectMouseEvents(dragMouseArea)

                // Setting just so the unit-tests can access the buttons
                tabBarCpp.tabBarQmlItem = this;
            }
        }

        Repeater {
            model: root.frameCpp ? root.frameCpp.tabWidget.dockWidgetModel : 0
            TabButton {
                text: title
            }
        }
    }

    StackLayout {
        id: stackLayout
        anchors {
            left: parent ? parent.left : undefined
            right: parent ? parent.right : undefined
            top: (parent && tabbar.visible) ? tabbar.bottom : ((titleBar && titleBar.visible) ? titleBar.bottom
                                                                                              : parent ? parent.top : undefined)
            bottom: parent ? parent.bottom : undefined

            leftMargin: root.contentsMargin
            rightMargin: root.contentsMargin
            bottomMargin: root.contentsMargin
        }

        currentIndex: tabbar.currentIndex
    }
}
