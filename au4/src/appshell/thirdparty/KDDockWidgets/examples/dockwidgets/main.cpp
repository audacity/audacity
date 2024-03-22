/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MyWidget.h"
#include "MyMainWindow.h"
#include "MyFrameworkWidgetFactory.h"

#include <kddockwidgets/Config.h>

#include <QStyleFactory>
#include <QApplication>
#include <QDebug>
#include <QCommandLineParser>

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

    QCommandLineParser parser;
    parser.setApplicationDescription("KDDockWidgets example application");
    parser.addHelpOption();

    // Fusion looks better in general, but feel free to change
    qApp->setStyle(QStyleFactory::create(QStringLiteral("Fusion")));

    QCommandLineOption customStyle("p", QCoreApplication::translate("main", "Shows how to style framework internals via FrameworkWidgetFactory"));
    parser.addOption(customStyle);

    QCommandLineOption reorderTabsOption("r", QCoreApplication::translate("main", "Support re-ordering tabs with mouse"));
    parser.addOption(reorderTabsOption);

    QCommandLineOption noTitleBars("t", QCoreApplication::translate("main", "Hide titlebars when tabs are visible"));
    parser.addOption(noTitleBars);

    QCommandLineOption alwaysTitleBarWhenFloating("q", QCoreApplication::translate("main", "Don't hide title bars if floating, even if Flag_HideTitleBarWhenTabsVisible is specified."));
    parser.addOption(alwaysTitleBarWhenFloating);

    QCommandLineOption alwaysTabs("z", QCoreApplication::translate("main", "Show tabs even if there's only one"));
    parser.addOption(alwaysTabs);

    QCommandLineOption lazyResizeOption("l", QCoreApplication::translate("main", "Use lazy resize"));
    parser.addOption(lazyResizeOption);

    QCommandLineOption multipleMainWindows("m", QCoreApplication::translate("main", "Shows two multiple main windows"));
    parser.addOption(multipleMainWindows);

    QCommandLineOption incompatibleMainWindows("i", QCoreApplication::translate("main", "Only usable with -m. Make the two main windows incompatible with each other. (Illustrates (MainWindowBase::setAffinityName))"));
    parser.addOption(incompatibleMainWindows);

    QCommandLineOption tabsHaveCloseButton("c", QCoreApplication::translate("main", "Tabs have a close button"));
    parser.addOption(tabsHaveCloseButton);

    QCommandLineOption nonClosableDockWidget("n", QCoreApplication::translate("main", "DockWidget #0 will be non-closable"));
    parser.addOption(nonClosableDockWidget);

    QCommandLineOption relativeRestore("s", QCoreApplication::translate("main", "Don't restore main window geometry, restore dock widgets in relative sizes"));
    parser.addOption(relativeRestore);

    QCommandLineOption doubleClickMaximize("x", QCoreApplication::translate("main", "Double clicking a title bar will maximize a floating window"));
    parser.addOption(doubleClickMaximize);

    QCommandLineOption nonDockable("d", QCoreApplication::translate("main", "DockWidget #9 will be non-dockable"));
    parser.addOption(nonDockable);

    QCommandLineOption maximizeButtonOption("b", QCoreApplication::translate("main", "Floating dockWidgets have maximize/restore buttons instead of float/dock button"));
    parser.addOption(maximizeButtonOption);

    QCommandLineOption minimizeButtonOption("k", QCoreApplication::translate("main", "Floating dockWidgets have a minimize button. Implies not being an utility window (~Qt::Tool)"));
    parser.addOption(minimizeButtonOption);

    QCommandLineOption segmentedIndicators("y", QCoreApplication::translate("main", "Use segmented indicators instead of classical"));
    parser.addOption(segmentedIndicators);

    QCommandLineOption noUtilityWindows("u", QCoreApplication::translate("main", "FloatingWindows will be normal windows instead of utility windows"));
    parser.addOption(noUtilityWindows);

    QCommandLineOption keepAbove("o", QCoreApplication::translate("main", "FloatingWindows will have Qt::WindowStaysOnTopHint. Implies not being an utility window (try it with -u too)"));
    parser.addOption(keepAbove);

    parser.addPositionalArgument("savedlayout", QCoreApplication::translate("main", "loads the specified json file at startup"));

#ifdef KDDOCKWIDGETS_SUPPORTS_NESTED_MAINWINDOWS
    QCommandLineOption dockableMainWindows("j", QCoreApplication::translate("main", "Allow main windows to be docked inside other main windows"));
    parser.addOption(dockableMainWindows);
#endif

    QCommandLineOption maxSizeOption("g", QCoreApplication::translate("main", "Make dock #8 have a max-size of 200x200."));
    parser.addOption(maxSizeOption);

    QCommandLineOption centralFrame("f", QCoreApplication::translate("main", "Persistent central frame"));

    QCommandLineOption autoHideSupport("w", QCoreApplication::translate("main", "Enables auto-hide/minimization to side-bar support"));
    parser.addOption(autoHideSupport);

    QCommandLineOption closeOnlyCurrentTab("close-only-current-tab",
                                           QCoreApplication::translate("main", "The title bar's close button will only close the current tab instead of all. Illustrates using Config::Flag_CloseOnlyCurrentTab"));
    parser.addOption(closeOnlyCurrentTab);

    QCommandLineOption dontCloseBeforeRestore("dont-close-widget-before-restore", //krazy:exclude=spelling
                                              QCoreApplication::translate("main", "DockWidget #5 won't be closed before a restore. Illustrates LayoutSaverOption::DontCloseBeforeRestore"));
    parser.addOption(dontCloseBeforeRestore);

    QCommandLineOption showButtonsInTabBarIfTitleBarHidden("show-buttons-in-tabbar-if-titlebar-hidden",
                                                           QCoreApplication::translate("main", "If we're not using title bars we'll still show the close and float button in the tab bar"));
    parser.addOption(showButtonsInTabBarIfTitleBarHidden);

    QCommandLineOption centralWidget("central-widget",
                                     QCoreApplication::translate("main", "The main window will have a non-detachable central widget"));
    parser.addOption(centralWidget);

    QCommandLineOption ctxtMenuOnTabs("allow-switch-tabs-via-menu",
                                      QCoreApplication::translate("main", "Allow switching tabs via context menu in tabs area"));
    parser.addOption(ctxtMenuOnTabs);

#if defined(DOCKS_DEVELOPER_MODE)
    parser.addOption(centralFrame);

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
#else
    Q_UNUSED(centralFrame)
#endif

    parser.process(app);

    if (parser.isSet(customStyle)) {
        Config::self().setFrameworkWidgetFactory(new CustomWidgetFactory()); // Sets our custom factory

        // Increase the separator size, just for demo
        Config::self().setSeparatorThickness(10);
    }

    if (parser.isSet(segmentedIndicators))
        KDDockWidgets::DefaultWidgetFactory::s_dropIndicatorType = KDDockWidgets::DropIndicatorType::Segmented;

    MainWindowOptions options = MainWindowOption_None;
    auto flags = KDDockWidgets::Config::self().flags();
#if defined(DOCKS_DEVELOPER_MODE)
    auto internalFlags = KDDockWidgets::Config::self().internalFlags();

    options = parser.isSet(centralFrame) ? MainWindowOption_HasCentralFrame
                                         : MainWindowOption_None;

    if (parser.isSet(centralWidget))
        options |= MainWindowOption_HasCentralWidget;

    if (parser.isSet(noQtTool))
        internalFlags |= KDDockWidgets::Config::InternalFlag_DontUseQtToolWindowsForFloatingWindows;

    if (parser.isSet(keepAbove))
        flags |= KDDockWidgets::Config::Flag_KeepAboveIfNotUtilityWindow;

    if (parser.isSet(noParentForFloating))
        internalFlags |= KDDockWidgets::Config::InternalFlag_DontUseParentForFloatingWindows;

    if (parser.isSet(nativeTitleBar))
        flags |= KDDockWidgets::Config::Flag_NativeTitleBar;

    if (parser.isSet(noDropIndicators))
        KDDockWidgets::DefaultWidgetFactory::s_dropIndicatorType = KDDockWidgets::DropIndicatorType::None;

# if defined(Q_OS_WIN)
    if (parser.isSet(noAeroSnap))
        internalFlags |= KDDockWidgets::Config::InternalFlag_NoAeroSnap;
# endif
    Config::self().setInternalFlags(internalFlags);
#endif

    if (parser.isSet(autoHideSupport))
        flags |= Config::Flag_AutoHideSupport;

    if (parser.isSet(closeOnlyCurrentTab))
        flags |= Config::Flag_CloseOnlyCurrentTab;

    if (parser.isSet(showButtonsInTabBarIfTitleBarHidden))
        flags |= Config::Flag_ShowButtonsOnTabBarIfTitleBarHidden;

    if (parser.isSet(noTitleBars))
        flags |= KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible;

    if (parser.isSet(noUtilityWindows))
        flags |= KDDockWidgets::Config::Flag_DontUseUtilityFloatingWindows;

    if (parser.isSet(alwaysTabs))
        flags |= KDDockWidgets::Config::Flag_AlwaysShowTabs;

    if (parser.isSet(alwaysTitleBarWhenFloating)) {
        flags |= KDDockWidgets::Config::Flag_AlwaysTitleBarWhenFloating;
        if (!(flags & KDDockWidgets::Config::Flag_HideTitleBarWhenTabsVisible)) {
            qWarning() << "Flag_AlwaysTitleBarWhenFloating is unneeded if Flag_HideTitleBarWhenTabsVisible isn't used."
                       << "As floating windows already have title bars by default.";
        }
    }

    if (parser.isSet(customStyle) || qEnvironmentVariableIsSet("KDDW_DEBUG_FOCUS"))
        flags |= KDDockWidgets::Config::Flag_TitleBarIsFocusable; // also showing title bar focus with -p, just to not introduce another switch

    if (parser.isSet(reorderTabsOption))
        flags |= KDDockWidgets::Config::Flag_AllowReorderTabs;

    if (parser.isSet(maximizeButtonOption))
        flags |= KDDockWidgets::Config::Flag_TitleBarHasMaximizeButton;

    if (parser.isSet(minimizeButtonOption))
        flags |= KDDockWidgets::Config::Flag_TitleBarHasMinimizeButton;

    if (parser.isSet(lazyResizeOption))
        flags |= KDDockWidgets::Config::Flag_LazyResize;

    if (parser.isSet(tabsHaveCloseButton))
        flags |= KDDockWidgets::Config::Flag_TabsHaveCloseButton;

    if (parser.isSet(ctxtMenuOnTabs))
        flags |= KDDockWidgets::Config::Flag_AllowSwitchingTabsViaMenu;


    if (parser.isSet(doubleClickMaximize))
        flags |= KDDockWidgets::Config::Flag_DoubleClickMaximizes;

    if (parser.isSet(incompatibleMainWindows) && !parser.isSet(multipleMainWindows)) {
        qWarning() << "Error: Argument -i requires -m";
        return 1;
    }

    KDDockWidgets::Config::self().setFlags(flags);

    const bool nonClosableDockWidget0 = parser.isSet(nonClosableDockWidget);
    const bool restoreIsRelative = parser.isSet(relativeRestore);
    const bool nonDockableDockWidget9 = parser.isSet(nonDockable);
    const bool maxSizeForDockWidget8 = parser.isSet(maxSizeOption);
    const bool dontCloseDockWidget5BeforeRestore = parser.isSet(dontCloseBeforeRestore);
    const bool usesMainWindowsWithAffinity = parser.isSet(multipleMainWindows);

#ifdef KDDOCKWIDGETS_SUPPORTS_NESTED_MAINWINDOWS
    const bool usesDockableMainWindows = parser.isSet(dockableMainWindows);
#else
    const bool usesDockableMainWindows = false;
#endif

    MyMainWindow mainWindow(QStringLiteral("MyMainWindow"), options, nonClosableDockWidget0,
                            nonDockableDockWidget9, restoreIsRelative, maxSizeForDockWidget8,
                            dontCloseDockWidget5BeforeRestore);
    mainWindow.setWindowTitle("Main Window 1");
    mainWindow.resize(1200, 1200);
    mainWindow.show();

    if (usesMainWindowsWithAffinity) {
        if (usesDockableMainWindows) {
            qWarning() << "MainWindows with affinity option is incompatible with Dockable Main Windows option";
            return 1;
        }

        // By default a dock widget can dock into any main window.
        // By setting an affinity name we can prevent that. Dock widgets of different affinities are incompatible.
        const QString affinity = parser.isSet(incompatibleMainWindows) ? QStringLiteral("affinity1")
                                                                       : QString();

        auto mainWindow2 = new MyMainWindow(QStringLiteral("MyMainWindow-2"), options,
                                            nonClosableDockWidget0, nonDockableDockWidget9,
                                            restoreIsRelative, maxSizeForDockWidget8,
                                            dontCloseDockWidget5BeforeRestore, affinity);
        if (affinity.isEmpty())
            mainWindow2->setWindowTitle("Main Window 2");
        else
            mainWindow2->setWindowTitle("Main Window 2 (different affinity)");

        mainWindow2->resize(1200, 1200);
        mainWindow2->show();
    } else if (usesDockableMainWindows) {
        auto mainWindowDockWidget = new DockWidget(QStringLiteral("MyMainWindow-2-DW"));

        const QString affinity = QStringLiteral("Inner-DockWidgets-2");
        auto dockableMainWindow = new MyMainWindow(QStringLiteral("MyMainWindow-2"), options,
                                                   false, false, restoreIsRelative, false,
                                                   false, affinity);

        dockableMainWindow->setAffinities({ affinity });

        dockableMainWindow->setStyleSheet(QStringLiteral("background: yellow"));

        dockableMainWindow->setWindowTitle("Dockable Main Window");
        dockableMainWindow->show();
        mainWindowDockWidget->setWidget(dockableMainWindow);
        mainWindowDockWidget->show();
        mainWindowDockWidget->resize(800, 800);
    }

    const QStringList args = parser.positionalArguments();
    if (args.size() >= 1) {
        const QString sourceJsonFileName = args[0];
        KDDockWidgets::LayoutSaver loader;
        if (!loader.restoreFromFile(sourceJsonFileName)) {
            qWarning() << Q_FUNC_INFO << "Failed to restore from" << sourceJsonFileName;
            return 1;
        }
    }


    return app.exec();
}
