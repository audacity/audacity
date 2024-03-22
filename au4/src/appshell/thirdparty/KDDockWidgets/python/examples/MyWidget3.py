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

class MyWidget3(MyWidget):

    def __init__(self, parent = None):
        super().__init__(":/assets/base.png", ":/assets/KDAB_bubble_fulcolor.png", parent)
        self.m_triangle = QtGui.QImage(":/assets/tri.png")

    def paintEvent(self, ev):
        p = QtGui.QPainter(self)
        p.fillRect(self.rect(), QtGui.QColor(0xD5, 0xD5, 0xD5))
        p.drawImage(self.m_background.rect(), self.m_background, self.m_background.rect())
        
        targetRect = QtCore.QRect(QtCore.QPoint(self.width() - self.m_triangle.width(), self.height() - self.m_triangle.height()), self.m_triangle.size())

        self.drawLogo(p)

