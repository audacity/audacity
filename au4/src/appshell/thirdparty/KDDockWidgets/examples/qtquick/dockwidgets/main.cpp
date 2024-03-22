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

#include <QQmlApplicationEngine>
#include <QGuiApplication>
#include <QCommandLineParser>

int main(int argc, char *argv[])
{
#ifdef Q_OS_WIN
    QGuiApplication::setAttribute(Qt::AA_UseOpenGLES);
#endif
#if QT_VERSION < QT_VERSION_CHECK(6, 0, 0)
    QGuiApplication::setAttribute(Qt::AA_EnableHighDpiScaling);
    QGuiApplication::setAttribute(Qt::AA_UseHighDpiPixmaps);
#endif
    QGuiApplication app(argc, argv);
    QCommandLineParser parser;
    parser.setApplicationDescription("KDDockWidgets example application");
    parser.addHelpOption();


#if defined(DOCKS_DEVELOPER_MODE)
    QCommandLineOption noQtTool("no-qttool", QCoreApplication::translate("main", "(internal) Don't use Qt::Tool"));
    QCommandLineOption noParentForFloating("no-parent-for-floating", QCoreApplication::translate("main", "(internal) FloatingWindows won't have a parent"));
    QCommandLineOption nativeTitleBar("native-title-bar", QCoreApplication::translate("main", "(internal) FloatingWindows a native title bar"));
    QCommandLineOption noDropIndicators("no-drop-indicators", QCoreApplication::translate("main", "(internal) Don't use any drop indicators"));

    parser.addOption(noQtTool);
    parser.addOption(noParentForFloating);
    parser.addOption(nativeTitleBar);
    parser.addOption(noDropIndicators);

# if defined(Q_OS_WIN)
    QCommandLineOption noAeroSnap("no-aero-snap", QCoreApplication::translate("main", "(internal) Disable AeroSnap"));
    parser.addOption(noAeroSnap);
# endif
#endif

    auto flags = KDDockWidgets::Config::self().flags();

#if defined(DOCKS_DEVELOPER_MODE)
    auto internalFlags = KDDockWidgets::Config::self().internalFlags();
    parser.process(app);

    if (parser.isSet(noQtTool))
        internalFlags |= KDDockWidgets::Config::InternalFlag_DontUseQtToolWindowsForFloatingWindows;

    if (parser.isSet(noParentForFloating))
        internalFlags |= KDDockWidgets::Config::InternalFlag_DontUseParentForFloatingWindows;

    if (parser.isSet(nativeTitleBar))
        flags |= KDDockWidgets::Config::Flag_NativeTitleBar;
    else if (parser.isSet(noDropIndicators))
        KDDockWidgets::DefaultWidgetFactory::s_dropIndicatorType = KDDockWidgets::DropIndicatorType::None;

# if defined(Q_OS_WIN)
    if (parser.isSet(noAeroSnap))
        internalFlags |= KDDockWidgets::Config::InternalFlag_NoAeroSnap;
# endif

    // These are debug-only/development flags, which you can ignore.
    KDDockWidgets::Config::self().setInternalFlags(internalFlags);
#endif

    // Set any required flags. The defaults are usually fine.
    KDDockWidgets::Config::self().setFlags(flags);

    // Create your engine which loads main.qml. A simple QQuickView would work too.
    QQmlApplicationEngine appEngine;
    KDDockWidgets::Config::self().setQmlEngine(&appEngine);
    appEngine.load((QUrl("qrc:/main.qml")));

    // Below we illustrate usage of our C++ API. Alternative you can use declarative API.
    // See main.qml for examples of dockwidgets created directly in QML

    auto dw1 = new KDDockWidgets::DockWidgetQuick("Dock #1");

    dw1->setWidget(QStringLiteral("qrc:/Guest1.qml"));
    dw1->resize(QSize(800, 800));
    dw1->show();

    auto dw2 = new KDDockWidgets::DockWidgetQuick("Dock #2");
    dw2->setWidget(QStringLiteral("qrc:/Guest2.qml"));
    dw2->resize(QSize(800, 800));
    dw2->show();

    auto dw3 = new KDDockWidgets::DockWidgetQuick("Dock #3");
    dw3->setWidget(QStringLiteral("qrc:/Guest3.qml"));

    dw1->addDockWidgetToContainingWindow(dw3, KDDockWidgets::Location_OnRight);

    KDDockWidgets::MainWindowBase *mainWindow = KDDockWidgets::DockRegistry::self()->mainwindows().constFirst();
    mainWindow->addDockWidget(dw2, KDDockWidgets::Location_OnTop);

    return app.exec();
}
