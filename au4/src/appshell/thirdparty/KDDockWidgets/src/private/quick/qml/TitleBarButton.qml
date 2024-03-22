/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.9

Rectangle {
    id: root

    signal clicked()
    property alias imageSource: image.source

    color: "transparent"
    height: 16
    width: 16

    radius: 3
    border {
        color: "#666666"
        width: mouseArea.containsMouse ? 1 : 0
    }

    Image {
        id: image
        anchors.centerIn: parent
        anchors {
            verticalCenterOffset: 1
            horizontalCenterOffset: 1
        }
    }

    MouseArea {
        id: mouseArea
        hoverEnabled: true
        anchors.fill: parent
        onClicked: {
            root.clicked();
        }
    }
}
