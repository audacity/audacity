/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sergio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

import QtQuick 2.9

Item {
    anchors.fill: parent

    property alias background: background.source
    property alias logo: logo.source


    Image {
        id: background
        anchors.fill: parent
        fillMode: Image.PreserveAspectCrop

        Image {
            id: logo

            fillMode: Image.PreserveAspectFit
            anchors {
                fill: parent
                margins: 50
            }
        }
    }
}
