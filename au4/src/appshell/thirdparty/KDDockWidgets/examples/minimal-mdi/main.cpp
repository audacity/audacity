/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MyWidget.h"

#include <kddockwidgets/DockWidget.h>
#include <kddockwidgets/MainWindowMDI.h>

#include <QStyleFactory>
#include <QApplication>

// clazy:excludeall=qstring-allocations

int main(int argc, char **argv)
{
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
    QApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    QApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);
#endif
    QApplication app(argc, argv);

    app.setOrganizationName(QStringLiteral("KDAB"));
    app.setApplicationName(QStringLiteral("Test app"));

    // Fusion looks better in general, but feel free to change
    qApp->setStyle(QStyleFactory::create(QStringLiteral("Fusion")));

    // # 1. Create our main window

    KDDockWidgets::MainWindowMDI mainWindow(QStringLiteral("MyMainWindow"));
    mainWindow.setWindowTitle("Main Window");
    mainWindow.resize(1200, 1200);
    mainWindow.show();

    // # 2. Create a dock widget, it needs a unique name
    auto dock1 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock1"));
    auto widget1 = new MyWidget1();
    dock1->setWidget(widget1);

    auto dock2 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock2"));
    auto widget2 = new MyWidget2();
    dock2->setWidget(widget2);

    auto dock3 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock3"));
    auto widget3 = new MyWidget3();
    dock3->setWidget(widget3);

    // # 3. Dock them
    mainWindow.addDockWidget(dock1, QPoint(10, 10));
    mainWindow.addDockWidget(dock2, QPoint(50, 50));
    mainWindow.addDockWidget(dock3, QPoint(90, 90));

    return app.exec();
}
