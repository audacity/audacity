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

from PySide2 import QtCore, QtWidgets, QtGui

from MyWidget1 import MyWidget1
from MyWidget2 import MyWidget2
from MyWidget3 import MyWidget3

def newMyWidget(parent = None):
    randomNumber = QtCore.QRandomGenerator.global_().bounded(0, 100) + 1
    if (randomNumber < 50):
        if (randomNumber < 33):
            return MyWidget1(parent)
        else:
            return MyWidget3(parent)
    else:
        return MyWidget2(parent)

class MyMainWindow(KDDockWidgets.MainWindow):
    s_count = 0
    s_menuCount = 0

    def __init__(self, uniqueName, options = KDDockWidgets.MainWindowOption_None, dockWidget0IsNonClosable = False, nonDockableDockWidget9 = False, restoreIsRelative = False, maxSizeForDockWidget8 = False, affinityName = "", parent = None):
        super().__init__(uniqueName, options, parent)
        self.m_dockWidget0IsNonClosable = dockWidget0IsNonClosable
        self.m_dockWidget9IsNonDockable = nonDockableDockWidget9
        self.m_restoreIsRelative = restoreIsRelative
        self.m_maxSizeForDockWidget8 = maxSizeForDockWidget8
        self.m_dockwidgets = []
        
        menubar = self.menuBar()
        fileMenu = QtWidgets.QMenu("File")
        self.m_toggleMenu = QtWidgets.QMenu("Toggle")
        menubar.addMenu(fileMenu)
        menubar.addMenu(self.m_toggleMenu)

        newAction = fileMenu.addAction("New DockWidget")
        newAction.triggered.connect(self._newDockWidget)

        saveLayoutAction = fileMenu.addAction("Save Layout")
        saveLayoutAction.triggered.connect(self._saveLayout)

        restoreLayoutAction = fileMenu.addAction("Restore Layout")
        restoreLayoutAction.triggered.connect(self._restoreLayout)

        closeAllAction = fileMenu.addAction("Close All")
        closeAllAction.triggered.connect(self._closeAll)

        layoutEqually = fileMenu.addAction("Layout Equally")
        layoutEqually.triggered.connect(self.layoutEqually)

        quitAction = fileMenu.addAction("Quit")
        quitAction.triggered.connect(QtWidgets.QApplication.instance().quit)

        self.setAffinities([ affinityName ])
        self.createDockWidgets()

    def _newDockWidget(self):
        MyMainWindow.s_menuCount += 1
        w = newMyWidget(self)
        w.setGeometry(100, 100, 400, 400)
        dock = KDDockWidgets.DockWidget("new dock %d"%(MyMainWindow.s_menuCount))
        dock.setWidget(w)
        dock.resize(600, 600)
        dock.show()
        self.m_dockwidgets.append(dock)
        
    def _saveLayout(self):
        #saver = KDDockWidgets.LayoutSaver()
        #result = saver.saveToFile("mylayout.json")
        #print("Saving layout to disk. Result=", result)
        print("Not available")

    def _restoreLayout(self):
        #options = KDDockWidgets.RestoreOption_None
        #if self.m_restoreIsRelative:
        #    options |= KDDockWidgets.RestoreOption_RelativeToMainWindow
        #saver = KDDockWidgets.LayoutSaver(options)
        #saver.restoreFromFile("mylayout.json")
        print("Not available")

    def _closeAll(self):
        for dw in self.m_dockwidgets:
            dw.close()

    def createDockWidgets(self):
        if self.m_dockWidget9IsNonDockable:
            numDockWidgets = 10
        else:
            numDockWidgets = 9

        # numDockWidgets = 2
        # Create 9 KDDockWidget::DockWidget and the respective widgets they're hosting (MyWidget instances)
        for i in range(numDockWidgets):
            self.m_dockwidgets.append(self.newDockWidget())

        # MainWindow::addDockWidget() attaches a dock widget to the main window:
        initialOpts = KDDockWidgets.InitialOption(KDDockWidgets.InitialVisibilityOption.StartHidden, QtCore.QSize(500, 500))
        self.addDockWidget(self.m_dockwidgets[0], KDDockWidgets.Location_OnBottom, None, initialOpts)

        # Here, for finer granularity we specify right of dockwidgets[0]:
        self.addDockWidget(self.m_dockwidgets[1], KDDockWidgets.Location_OnRight, self.m_dockwidgets[0])
        
        self.addDockWidget(self.m_dockwidgets[2], KDDockWidgets.Location_OnLeft)
        self.addDockWidget(self.m_dockwidgets[3], KDDockWidgets.Location_OnBottom)
        self.addDockWidget(self.m_dockwidgets[4], KDDockWidgets.Location_OnBottom)

        # Tab two dock widgets together
        self.m_dockwidgets[3].addDockWidgetAsTab(self.m_dockwidgets[5])

        # 6 is floating, as it wasn't added to the main window via MainWindow::addDockWidget().
        # and we tab 7 with it.
        self.m_dockwidgets[6].addDockWidgetAsTab(self.m_dockwidgets[7])

        # Floating windows also support nesting, here we add 8 to the bottom of the group
        self.m_dockwidgets[6].addDockWidgetToContainingWindow(self.m_dockwidgets[8], KDDockWidgets.Location_OnBottom)

        floatingWindow = self.m_dockwidgets[6].window()
        floatingWindow.move(100, 100)

    def newDockWidget(self):
        # Passing options is optional, we just want to illustrate Option_NotClosable here
        options = KDDockWidgets.DockWidget.Option_None
        if (MyMainWindow.s_count == 0) and self.m_dockWidget0IsNonClosable:
            options |= KDDockWidgets.DockWidget.Option_NotClosable

        if (MyMainWindow.s_count == 9) and self.m_dockWidget9IsNonDockable:
            options |= KDDockWidgets.DockWidget.Option_NotDockable

        dock = KDDockWidgets.DockWidget("DockWidget #%d"%(MyMainWindow.s_count), options)
        dock.setAffinities(self.affinities()); # optional, just to show the feature. Pass -mi to the example to see incompatible dock widgets

        if MyMainWindow.s_count == 1:
            dock.setIcon(QtGui.QIcon.fromTheme("mail-message"))

        myWidget = newMyWidget(self)
        if (MyMainWindow.s_count == 8) and self.m_maxSizeForDockWidget8:
            # Set a maximum size on dock #8
            myWidget.setMaximumSize(200, 200)

        dock.setWidget(myWidget)

        if dock.options() & KDDockWidgets.DockWidget.Option_NotDockable:
            dock.setTitle("DockWidget #%d (%s)" %(MyMainWindow.s_count, "non dockable"))
        else:
            dock.setTitle("DockWidget #%d"%(MyMainWindow.s_count))

        dock.resize(600, 600)
        self.m_toggleMenu.addAction(dock.toggleAction())
        MyMainWindow.s_count += 1
        return dock

