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

class MyWidget(QtWidgets.QWidget):
    s_images = {}
    def __init__(self, backgroundFile, logoFile, parent = None):
        super().__init__(parent)

        self.m_background = self._lookupImage(backgroundFile)
        self.m_logo = self._lookupImage(logoFile)

    def _lookupImage(self, imageName):
        if imageName == "":
            return None

        if imageName not in MyWidget.s_images:
            MyWidget.s_images[imageName] = QtGui.QImage(imageName)

        return MyWidget.s_images[imageName]
        
    def drawLogo(self, p):

        if not self.m_logo:
            return

        ratio = self.m_logo.height() / (self.m_logo.width() * 1.0)
        maxWidth = int(0.80 * self.size().width())
        maxHeight = int(0.80 * self.size().height())
        proposedHeight = int(maxWidth * ratio)
        if (proposedHeight <= maxHeight):
            width = maxWidth
        else:
            width = int(maxHeight / ratio)

        height = int(width * ratio)
        targetLogoRect = QtCore.QRect(0,0, width, height)
        targetLogoRect.moveCenter(self.rect().center() + QtCore.QPoint(0, -int(self.size().height() * 0.00)))
        p.drawImage(targetLogoRect, self.m_logo, self.m_logo.rect());

