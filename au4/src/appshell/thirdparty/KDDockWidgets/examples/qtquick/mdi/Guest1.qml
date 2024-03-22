/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sergio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.9
import QtQuick.Controls 2.12
import com.kdab.dockwidgets 1.0 as KDDW

Guest {
    anchors.fill: parent
    background: "qrc:/assets/triangles.png"
    logo: "qrc:/assets/KDAB_bubble_white.png"

    KDDW.DockWidget {
        id: another
        uniqueName: "another1"
        source: ":/Another.qml"
    }

    Button {
        text: "Toggle Another"
        anchors {
            bottom: parent.bottom
            left: parent.left
            margins: 5
        }

        onClicked: {

            if (another.dockWidget.visible) {
                another.dockWidget.close();
            } else {
                another.dockWidget.show();
            }
        }
    }
}
