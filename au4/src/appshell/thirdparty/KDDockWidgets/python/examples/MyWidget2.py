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

import PyKDDockWidgets

from PySide2 import QtWidgets, QtGui, QtCore
    
from MyWidget import MyWidget

class MyWidget2(MyWidget):

    def __init__(self, parent = None):
        super().__init__("", ":/assets/KDAB_bubble_blue.png", parent)

    def paintEvent(self, ev):
        p = QtGui.QPainter(self)
        p.fillRect(self.rect(), QtCore.Qt.white);
        self.drawLogo(p)

