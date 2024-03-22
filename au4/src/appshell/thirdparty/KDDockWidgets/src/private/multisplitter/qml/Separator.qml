/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.6

Rectangle {
    id: root
    anchors.fill: parent
    color: "#eff0f1"

    readonly property QtObject kddwSeparator: parent

    MouseArea {
        cursorShape: kddwSeparator ? (kddwSeparator.isVertical ? Qt.SizeVerCursor : Qt.SizeHorCursor)
                                   : Qt.SizeHorCursor
        anchors.fill: parent
        onPressed: {
            kddwSeparator.onMousePressed();
        }

        onReleased: {
            kddwSeparator.onMouseReleased();
        }

        onPositionChanged: (mouse) => {
            kddwSeparator.onMouseMoved(Qt.point(mouse.x, mouse.y));
        }

        onDoubleClicked: {
            kddwSeparator.onMouseDoubleClicked();
        }
    }
}
