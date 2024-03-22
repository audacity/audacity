/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.9
import "." as KDDW

Rectangle {
    id: root
    readonly property QtObject floatingWindowCpp: parent
    readonly property QtObject titleBarCpp: floatingWindowCpp ? floatingWindowCpp.titleBar : null
    readonly property QtObject dropAreaCpp: floatingWindowCpp ? floatingWindowCpp.dropArea : null
    readonly property int titleBarHeight: titleBar.heightWhenVisible
    property int margins: 4

    anchors.fill: parent

    color: "transparent"
    border {
        color: "#666666"
        width: 1
    }

    onTitleBarHeightChanged: {
        if (floatingWindowCpp)
            floatingWindowCpp.geometryUpdated();
    }

    Loader {
        id: titleBar
        readonly property QtObject titleBarCpp: root.titleBarCpp
        readonly property int heightWhenVisible: item.heightWhenVisible
        source: _kddw_widgetFactory.titleBarFilename()

        anchors {
            top:  parent ? parent.top : undefined
            left: parent ? parent.left : undefined
            right: parent ? parent.right : undefined
            margins: root.margins
        }
    }

    KDDW.DropArea {
        id: dropArea
        dropAreaCpp: root.dropAreaCpp
        anchors {
            left: parent ? parent.left : undefined
            right: parent ? parent.right : undefined
            top: titleBar.bottom
            bottom: parent ? parent.bottom : undefined

            leftMargin: root.margins
            rightMargin: root.margins
            bottomMargin: root.margins
        }

        onHeightChanged: {
            // console.log("FloatingWindow.qml.dropArea height changed to " + height + " ; root.height= " + root.height)
        }
    }

    onDropAreaCppChanged: {
        // Parent the cpp obj to the visual obj. So the user can style it
        if (dropAreaCpp) {
            //console.log("Setup start: height=" + height + "; dropArea.height=" + dropAreaCpp.height);
            dropAreaCpp.parent = dropArea;
            dropAreaCpp.anchors.fill = dropArea;
            //console.log("Setup done: height=" + height + "; dropArea.height=" + dropAreaCpp.height);
        }
    }

    onHeightChanged: {
        // console.log("FloatingWindow.qml.root height changed to " + height)
    }

    onWidthChanged: {
        // console.log("FloatingWindow.qml.root width changed to " + width)
    }
}
