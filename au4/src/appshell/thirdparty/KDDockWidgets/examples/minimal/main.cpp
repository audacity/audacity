/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MyWidget.h"

#include <kddockwidgets/DockWidget.h>
#include <kddockwidgets/MainWindow.h>

#include <QStyleFactory>
#include <QApplication>

// clazy:excludeall=qstring-allocations

using namespace KDDockWidgets;

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

    KDDockWidgets::MainWindow mainWindow(QStringLiteral("MyMainWindow"));
    mainWindow.setWindowTitle("Main Window");
    mainWindow.resize(1200, 1200);
    mainWindow.show();

    // # 2. Create a dock widget, it needs a unique name
    auto dock1 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock1"));
    auto widget1 = new MyWidget();
    dock1->setWidget(widget1);

    auto dock2 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock2"));
    auto widget2 = new MyWidget(QStringLiteral(":/assets/base.png"),
                                QStringLiteral(":/assets/KDAB_bubble_fulcolor.png"));
    dock2->setWidget(widget2);

    auto dock3 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock3"));
    auto widget3 = new MyWidget(QStringLiteral(":/assets/base.png"),
                                QStringLiteral(":/assets/KDAB_bubble_fulcolor.png"));
    dock3->setWidget(widget3);

    auto dock4 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock4"));
    auto widget4 = new MyWidget(QStringLiteral(":/assets/base.png"),
                                QStringLiteral(":/assets/KDAB_bubble_fulcolor.png"));
    dock4->setWidget(widget4);

    auto dock5 = new KDDockWidgets::DockWidget(QStringLiteral("MyDock5"));
    auto widget5 = new MyWidget(QStringLiteral(":/assets/base.png"),
                                QStringLiteral(":/assets/KDAB_bubble_fulcolor.png"));
    dock5->setWidget(widget5);

    // 3. Add them to the main window
    mainWindow.addDockWidget(dock1, KDDockWidgets::Location_OnLeft);
    mainWindow.addDockWidget(dock2, KDDockWidgets::Location_OnTop);

    // 4. Add dock3 to the right of dock2
    mainWindow.addDockWidget(dock3, KDDockWidgets::Location_OnRight, dock2);

    // 5. dock4 is docked at the bottom, with 200px height
    const QSize preferredSize(QSize(/*ignored*/0, 200));
    mainWindow.addDockWidget(dock4, KDDockWidgets::Location_OnBottom, nullptr, preferredSize);


    // 5. dock5 will be its own top level (floating window)
    dock5->show();

    return app.exec();
}
