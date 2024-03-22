/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sergio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.6

// Will be moved to a plugin in the future
import "qrc:/kddockwidgets/private/quick/qml/" as KDDW

KDDW.TitleBarBase {
    id: root
    color: "black"
    border.color: "orange"
    border.width: 2
    heightWhenVisible: 50

    Text {
        color: "orange"
        font.bold: true
        text: root.title
        anchors {
            left: parent.left
            leftMargin: 10
            verticalCenter: root.verticalCenter
        }
    }

    Rectangle {
        id: closeButton
        enabled: root.closeButtonEnabled
        radius: 5
        color: "green"
        height: root.height - 20
        width: height
        anchors {
            right: root.right
            rightMargin: 10
            verticalCenter: root.verticalCenter
        }
        MouseArea {
            anchors.fill: parent
            onClicked: {
                root.closeButtonClicked();
            }
        }
    }
}
