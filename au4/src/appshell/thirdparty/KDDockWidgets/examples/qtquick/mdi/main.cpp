/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sergio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/


#include <kddockwidgets/Config.h>
#include <kddockwidgets/DockWidgetQuick.h>
#include <kddockwidgets/private/DockRegistry_p.h>
#include <kddockwidgets/FrameworkWidgetFactory.h>
#include <kddockwidgets/MainWindowMDI.h>

#include <QQmlApplicationEngine>
#include <QGuiApplication>
#include <QCommandLineParser>

// Foro my own debugging, until we have better API
#include "../../src/private/MDILayoutWidget_p.h"

int main(int argc, char *argv[])
{
#ifdef Q_OS_WIN
    QGuiApplication::setAttribute(Qt::AA_UseOpenGLES);
#endif
    QGuiApplication app(argc, argv);
    QCommandLineParser parser;
    parser.setApplicationDescription("KDDockWidgets example application");
    parser.addHelpOption();

    QQmlApplicationEngine appEngine;
    KDDockWidgets::Config::self().setQmlEngine(&appEngine);
    appEngine.load((QUrl("qrc:/main.qml")));

    auto dw1 = new KDDockWidgets::DockWidgetQuick("Dock #1");
    dw1->setWidget(QStringLiteral("qrc:/Guest1.qml"));
    dw1->resize(QSize(400, 400));

    auto dw2 = new KDDockWidgets::DockWidgetQuick("Dock #2");
    dw2->setWidget(QStringLiteral("qrc:/Guest2.qml"));
    dw2->resize(QSize(400, 400));

    auto dw3 = new KDDockWidgets::DockWidgetQuick("Dock #3");
    dw3->setWidget(QStringLiteral("qrc:/Guest3.qml"));

    auto mainWindow = static_cast<KDDockWidgets::MainWindowMDI*>(KDDockWidgets::DockRegistry::self()->mainwindows().constFirst());

    mainWindow->addDockWidget(dw1, QPoint(10, 10));
    mainWindow->addDockWidget(dw2, QPoint(50, 50));
    mainWindow->addDockWidget(dw3, QPoint(90, 90));


    return app.exec();
}
