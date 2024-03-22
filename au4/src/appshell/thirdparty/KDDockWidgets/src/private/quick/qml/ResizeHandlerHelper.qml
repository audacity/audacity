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

MouseArea {
    id: root
    required property int resizeMargin
    required property bool resizeAllowed
    required property QtObject frameCpp
    required property int cursorPosition

    hoverEnabled: true

    cursorShape: {
        if (!enabled) {
            // Even if disabled the MouseArea changes cursor, as it's different than Item.enabled, so explicitly change cursor if disabled
            return Qt.ArrowCursor;
        }

        if ((cursorPosition === KDDockWidgets.CursorPosition_TopLeft || cursorPosition === KDDockWidgets.CursorPosition_BottomRight)) {
            return Qt.SizeFDiagCursor;
        } else if ((cursorPosition === KDDockWidgets.CursorPosition_TopRight || cursorPosition === KDDockWidgets.CursorPosition_BottomLeft)) {
            return Qt.SizeBDiagCursor;
        } else if (cursorPosition & KDDockWidgets.CursorPosition_Horizontal) {
            return Qt.SizeHorCursor;
        } else if (cursorPosition & KDDockWidgets.CursorPosition_Vertical) {
            return Qt.SizeVerCursor;
        } else {
            return Qt.ArrowCursor;
        }
    }

    enabled: resizeAllowed

    onFrameCppChanged: {
        if (frameCpp) {
            // When Frame is in MDI mode, we need to detect when the mouse over the edges
            frameCpp.redirectMouseEvents(this)
        }
    }
}
