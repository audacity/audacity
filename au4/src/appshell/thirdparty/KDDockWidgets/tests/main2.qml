/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sergio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/// @brief A more involved QtQuick example for the tests
/// Copied from examples, but fine for tests too

import QtQuick 2.6
import QtQuick.Controls 2.12
import com.kdab.dockwidgets 1.0 as KDDW

ApplicationWindow {
    visible: true
    width: 1000
    height: 800

    KDDW.MainWindowLayout {
        anchors.fill: parent

        // Each main layout needs a unique id
        uniqueName: "MyWindowName-1"
    }
}
