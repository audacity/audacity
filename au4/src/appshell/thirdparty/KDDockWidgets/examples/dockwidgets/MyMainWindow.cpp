/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MyMainWindow.h"
#include "MyWidget.h"

#include <kddockwidgets/Config.h>
#include <kddockwidgets/LayoutSaver.h>

#include <QMenu>
#include <QMenuBar>
#include <QEvent>
#include <QDebug>
#include <QString>
#include <QTextEdit>
#include <QRandomGenerator>

#include <QApplication>

#include <stdlib.h>
#include <time.h>

// clazy:excludeall=qstring-allocations,ctor-missing-parent-argument,detaching-member

static MyWidget *newMyWidget()
{
    const int randomNumber = QRandomGenerator::global()->bounded(0, 100) + 1;
    if (randomNumber < 50) {
        if (randomNumber < 33) {
            return new MyWidget1();
        } else {
            return new MyWidget3();
        }
    } else {
        return new MyWidget2();
    }
}

MyMainWindow::MyMainWindow(const QString &uniqueName, KDDockWidgets::MainWindowOptions options,
                           bool dockWidget0IsNonClosable, bool nonDockableDockWidget9, bool restoreIsRelative,
                           bool maxSizeForDockWidget8, bool dockwidget5DoesntCloseBeforeRestore,
                           const QString &affinityName, QWidget *parent)
    : MainWindow(uniqueName, options, parent)
    , m_dockWidget0IsNonClosable(dockWidget0IsNonClosable)
    , m_dockWidget9IsNonDockable(nonDockableDockWidget9)
    , m_restoreIsRelative(restoreIsRelative)
    , m_maxSizeForDockWidget8(maxSizeForDockWidget8)
    , m_dockwidget5DoesntCloseBeforeRestore(dockwidget5DoesntCloseBeforeRestore)
{
    auto menubar = menuBar();
    auto fileMenu = new QMenu(QStringLiteral("File"), this);
    m_toggleMenu = new QMenu(QStringLiteral("Toggle"), this);
    auto miscMenu = new QMenu(QStringLiteral("Misc"), this);

    menubar->addMenu(fileMenu);
    menubar->addMenu(m_toggleMenu);
    menubar->addMenu(miscMenu);

    QAction *newAction = fileMenu->addAction(QStringLiteral("New DockWidget"));

    connect(newAction, &QAction::triggered, this, [] {
        static int count = 0;
        count++;
        auto w = newMyWidget();
        w->setGeometry(100, 100, 400, 400);
        auto dock = new KDDockWidgets::DockWidget(QStringLiteral("new dock %1").arg(count));
        dock->setWidget(w);
        dock->resize(600, 600);
        dock->show();
    });

    auto saveLayoutAction = fileMenu->addAction(QStringLiteral("Save Layout"));
    connect(saveLayoutAction, &QAction::triggered, this, [] {
        KDDockWidgets::LayoutSaver saver;
        const bool result = saver.saveToFile(QStringLiteral("mylayout.json"));
        qDebug() << "Saving layout to disk. Result=" << result;
    });

    auto restoreLayoutAction = fileMenu->addAction(QStringLiteral("Restore Layout"));
    connect(restoreLayoutAction, &QAction::triggered, this, [this] {
        KDDockWidgets::RestoreOptions options = KDDockWidgets::RestoreOption_None;
        if (m_restoreIsRelative)
            options |= KDDockWidgets::RestoreOption_RelativeToMainWindow;

        KDDockWidgets::LayoutSaver saver(options);
        saver.restoreFromFile(QStringLiteral("mylayout.json"));
    });

    auto closeAllAction = fileMenu->addAction(QStringLiteral("Close All"));
    connect(closeAllAction, &QAction::triggered, this, [this] {
        for (auto dw : std::as_const(m_dockwidgets))
            dw->close();
    });

    auto layoutEqually = fileMenu->addAction(QStringLiteral("Layout Equally"));
    connect(layoutEqually, &QAction::triggered, this, &MainWindow::layoutEqually);

    auto quitAction = fileMenu->addAction(QStringLiteral("Quit"));
    connect(quitAction, &QAction::triggered, qApp, &QApplication::quit);

    QAction *toggleDropIndicatorSupport = miscMenu->addAction(QStringLiteral("Toggle Drop Indicator Support"));
    toggleDropIndicatorSupport->setCheckable(true);
    toggleDropIndicatorSupport->setChecked(true);
    connect(toggleDropIndicatorSupport, &QAction::toggled, this, [](bool checked) {
        KDDockWidgets::Config::self().setDropIndicatorsInhibited(!checked);
    });

    setAffinities({ affinityName });
    createDockWidgets();

    if (options & KDDockWidgets::MainWindowOption_HasCentralWidget) {
        setPersistentCentralWidget(new MyWidget1());
    }
}

MyMainWindow::~MyMainWindow()
{
    qDeleteAll(m_dockwidgets);
}

void MyMainWindow::createDockWidgets()
{
    Q_ASSERT(m_dockwidgets.isEmpty());

    const int numDockWidgets = m_dockWidget9IsNonDockable ? 10 : 9;


    // Create 9 KDDockWidget::DockWidget and the respective widgets they're hosting (MyWidget instances)
    for (int i = 0; i < numDockWidgets; i++)
        m_dockwidgets << newDockWidget();

    // MainWindow::addDockWidget() attaches a dock widget to the main window:
    addDockWidget(m_dockwidgets.at(0), KDDockWidgets::Location_OnTop);

    // Here, for finer granularity we specify right of dockwidgets[0]:
    addDockWidget(m_dockwidgets.at(1), KDDockWidgets::Location_OnRight, m_dockwidgets.at(0));

    addDockWidget(m_dockwidgets.at(2), KDDockWidgets::Location_OnLeft);
    addDockWidget(m_dockwidgets.at(3), KDDockWidgets::Location_OnBottom);
    addDockWidget(m_dockwidgets.at(4), KDDockWidgets::Location_OnBottom);

    // Tab two dock widgets together
    m_dockwidgets[3]->addDockWidgetAsTab(m_dockwidgets.at(5));

    // 6 is floating, as it wasn't added to the main window via MainWindow::addDockWidget().
    // and we tab 7 with it.
    m_dockwidgets[6]->addDockWidgetAsTab(m_dockwidgets.at(7));

    // Floating windows also support nesting, here we add 8 to the bottom of the group
    m_dockwidgets[6]->addDockWidgetToContainingWindow(m_dockwidgets.at(8), KDDockWidgets::Location_OnBottom);

    auto floatingWindow = m_dockwidgets.at(6)->window();
    floatingWindow->move(100, 100);
}

KDDockWidgets::DockWidgetBase *MyMainWindow::newDockWidget()
{
    static int count = 0;

    // Passing options is optional, we just want to illustrate Option_NotClosable here
    KDDockWidgets::DockWidget::Options options = KDDockWidgets::DockWidget::Option_None;
    KDDockWidgets::DockWidget::LayoutSaverOptions layoutSaverOptions = KDDockWidgets::DockWidget::LayoutSaverOption::None;

    if (count == 0 && m_dockWidget0IsNonClosable)
        options |= KDDockWidgets::DockWidget::Option_NotClosable;

    if (count == 9 && m_dockWidget9IsNonDockable)
        options |= KDDockWidgets::DockWidget::Option_NotDockable;

    if (count == 5 && m_dockwidget5DoesntCloseBeforeRestore)
        layoutSaverOptions |= KDDockWidgets::DockWidget::LayoutSaverOption::Skip;

    auto dock = new KDDockWidgets::DockWidget(QStringLiteral("DockWidget #%1").arg(count), options, layoutSaverOptions);
    dock->setAffinities(affinities()); // optional, just to show the feature. Pass -mi to the example to see incompatible dock widgets

    if (count == 1)
        dock->setIcon(QIcon::fromTheme(QStringLiteral("mail-message")));

    auto myWidget = newMyWidget();
    if (count == 8 && m_maxSizeForDockWidget8) {
        // Set a maximum size on dock #8
        myWidget->setMaximumSize(200, 200);
    }

    dock->setWidget(myWidget);

    if (dock->options() & KDDockWidgets::DockWidget::Option_NotDockable) {
        dock->setTitle(QStringLiteral("DockWidget #%1 (%2)").arg(count).arg("non dockable"));
    } else {
        dock->setTitle(QStringLiteral("DockWidget #%1").arg(count));
    }

    dock->resize(600, 600);
    m_toggleMenu->addAction(dock->toggleAction());
    dock->toggleAction()->setShortcut(QStringLiteral("ctrl+%1").arg(count));

    count++;
    return dock;
}
