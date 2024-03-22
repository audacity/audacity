#
# This file is part of KDDockWidgets.
#
# SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
# Author: Renato Araujo Oliveira Filho <renato.araujo@kdab.com>
#
# SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only
#
# Contact KDAB at <info@kdab.com> for commercial licensing options.
#

from PyKDDockWidgets import KDDockWidgets
from MyMainWindow import MyMainWindow

from PySide2 import QtWidgets, QtCore

import sys
try:
    import rc_assets
except:
    exit("Oops.. rc_assets needs to be generated first.\nPlease run:\n rcc -g python -o rc_assets.py ../../examples/dockwidgets/resources_example.qrc\n(Make sure to use the rcc from the Qt5 version used to generate the bindings!)")

if __name__ == "__main__":
    QtWidgets.QApplication.setAttribute(QtCore.Qt.AA_EnableHighDpiScaling)
    QtWidgets.QApplication.setAttribute(QtCore.Qt.AA_UseHighDpiPixmaps)
    app = QtWidgets.QApplication(sys.argv)

    app.setOrganizationName("KDAB")
    app.setApplicationName("Test app")
    app.setStyle(QtWidgets.QStyleFactory.create("Fusion"))

    mainWindow = MyMainWindow("MyMainWindow", )
    mainWindow.setWindowTitle("Main Window 1")
    mainWindow.resize(1200, 1200)
    mainWindow.show()

    app.exec_()

