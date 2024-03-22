/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

// We don't care about performance related checks in the tests
// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates,qstring-allocations

#pragma once

#include "Testing.h"
#include "utils.h"

#include <QGuiApplication>
#include <QObject>
#include <QStyleFactory>
#include <QtTest/QtTest>

#ifdef KDDOCKWIDGETS_QTQUICK
# include "DockWidgetQuick.h"
# include "quick/MainWindowQuick_p.h"

# include <QQmlEngine>
# include <QQuickStyle>
# include <QQmlApplicationEngine>
#else
# include "DockWidget.h"
# include "MainWindow.h"

# include <QLineEdit>
# include <QMenuBar>
# include <QPushButton>
# include <QTabBar>
# include <QTextEdit>
#endif

using namespace KDDockWidgets::Tests;

class TestDocks : public QObject
{
    Q_OBJECT
public Q_SLOTS:
    void initTestCase()
    {
        qputenv("KDDOCKWIDGETS_SHOW_DEBUG_WINDOW", "");
        qApp->setOrganizationName("KDAB");
        qApp->setApplicationName("dockwidgets-unit-tests");

        qApp->setStyle(QStyleFactory::create("fusion"));
        KDDockWidgets::Testing::installFatalMessageHandler();

#ifdef KDDOCKWIDGETS_QTQUICK
        QQuickStyle::setStyle("Material"); // so we don't load KDE plugins
        KDDockWidgets::Config::self().setQmlEngine(new QQmlEngine(this));
#endif
    }

    void cleanupTestCase()
    {
#ifdef KDDOCKWIDGETS_QTQUICK
        delete KDDockWidgets::Config::self().qmlEngine();
#endif
    }

private Q_SLOTS:
    void tst_simple1();
    void tst_simple2();
    void tst_doesntHaveNativeTitleBar();
    void tst_resizeWindow2();
    void tst_hasLastDockedLocation();
    void tst_ghostSeparator();
    void tst_detachFromMainWindow();
    void tst_detachPos();
    void tst_floatingWindowSize();
    void tst_sizeAfterRedock();
    void tst_tabbingWithAffinities();
    void tst_honourUserGeometry();
    void tst_floatingWindowTitleBug();
    void tst_setFloatingSimple();
    void tst_dragOverTitleBar();
    void tst_setFloatingGeometry();

    void tst_resizeWindow_data();
    void tst_resizeWindow();
    void tst_restoreEmpty();
    void tst_restoreCentralFrame();
    void tst_restoreMaximizedState();
    void tst_restoreFloatingMaximizedState();
    void tst_restoreFloatingMinimizedState();
    void tst_restoreNonExistingDockWidget();
    void tst_shutdown();
    void tst_closeDockWidgets();
    void tst_layoutEqually();
    void tst_doubleClose();
    void tst_dockInternal();
    void tst_maximizeAndRestore();
    void tst_samePositionAfterHideRestore();
    void tst_restoreTwice();
    void tst_restoreAfterResize();
    void tst_restoreWithNonClosableWidget();
    void tst_restoreNestedAndTabbed();
    void tst_restoreCrash();
    void tst_restoreSideBySide();
    void tst_restoreWithCentralFrameWithTabs();
    void tst_restoreWithPlaceholder();
    void tst_restoreWithAffinity();
    void tst_marginsAfterRestore();
    void tst_restoreWithNewDockWidgets();
    void tst_restoreWithDockFactory();
    void tst_restoreWithDockFactory2();
    void tst_lastFloatingPositionIsRestored();
    void tst_restoreSimple();
    void tst_restoreSimplest();
    void tst_restoreNonClosable();
    void tst_restoreRestoresMainWindowPosition();
    void tst_invalidLayoutAfterRestore();
    void tst_dontCloseDockWidgetBeforeRestore();
    void tst_dontCloseDockWidgetBeforeRestore2();
    void tst_dontCloseDockWidgetBeforeRestore3();
    void tst_dontCloseDockWidgetBeforeRestore4();
    void tst_restoreWithNativeTitleBar();

    void tst_closeOnlyCurrentTab();
    void tst_tabWidgetCurrentIndex();
    void tst_doubleClickTabToDetach();
    void tst_propagateResize2();
    void tst_negativeAnchorPosition();
    void tst_negativeAnchorPosition2();
    void tst_negativeAnchorPosition3();
    void tst_negativeAnchorPosition4();
    void tst_negativeAnchorPosition5();
    void tst_negativeAnchorPosition6();
    void tst_negativeAnchorPosition7();
    void tst_startHidden();
    void tst_startHidden2();
    void tst_startClosed();
    void tst_closeReparentsToNull();
    void tst_invalidAnchorGroup();
    void tst_addAsPlaceholder();
    void tst_removeItem();
    void tst_clear();
    void tst_dockDockWidgetNested();
    void tst_dockFloatingWindowNested();
    void tst_crash();
    void tst_refUnrefItem();
    void tst_placeholderCount();
    void tst_availableLengthForOrientation();
    void tst_closeShowWhenNoCentralFrame();
    void tst_closeTabOfCentralFrame();
    void tst_centralFrame245();
    void tst_setAsCurrentTab();
    void tst_placeholderDisappearsOnReadd();
    void tst_placeholdersAreRemovedProperly();
    void tst_floatMaintainsSize();
    void tst_preferredInitialSize();

    void tst_crash2_data();
    void tst_crash2();
    void tst_closeAllDockWidgets();
    void tst_toggleMiddleDockCrash();
    void tst_stealFrame();
    void tst_setFloatingWhenWasTabbed();
    void tst_setFloatingWhenSideBySide();
    void tst_dockWindowWithTwoSideBySideFramesIntoCenter();
    void tst_tabTitleChanges();
    void tst_dockWidgetGetsFocusWhenDocked();
    void tst_setWidget();
    void tst_isFocused();
    void tst_floatingLastPosAfterDoubleClose();
    void tst_registry();
    void tst_honourGeometryOfHiddenWindow();
    void tst_0_data();
    void tst_0();
    void tst_dockWindowWithTwoSideBySideFramesIntoRight();
    void tst_dockWindowWithTwoSideBySideFramesIntoLeft();
    void tst_posAfterLeftDetach();
    void tst_preventClose();
    void tst_propagateMinSize();
    void tst_createFloatingWindow();
    void tst_addAndReadd();
    void tst_fairResizeAfterRemoveWidget();
    void tst_invalidJSON_data();
    void tst_invalidJSON();

    void tst_invalidPlaceholderPosition_data();
    void tst_invalidPlaceholderPosition();
    void tst_setVisibleFalseWhenSideBySide_data();
    void tst_setVisibleFalseWhenSideBySide();
    void tst_resizeViaAnchorsAfterPlaceholderCreation();
    void tst_rectForDropCrash();
    void tst_addDockWidgetToMainWindow();
    void tst_addDockWidgetToContainingWindow();
    void tst_notClosable();
    void tst_setFloatingAfterDraggedFromTabToSideBySide();
    void tst_setFloatingAFrameWithTabs();
    void tst_toggleDockWidgetWithHiddenTitleBar();
    void tst_tabBarWithHiddenTitleBar_data();
    void tst_tabBarWithHiddenTitleBar();
    void tst_availableSizeWithPlaceholders();
    void tst_anchorFollowingItselfAssert();
    void tst_moreTitleBarCornerCases();
    void tst_isInMainWindow();
    void tst_sizeConstraintWarning();
    void tst_stuckSeparator();
    void tst_dockNotFillingSpace();
    void tst_titlebar_getter();
    void tst_raise();
    void tst_addingOptionHiddenTabbed();
    void tst_maxSizedHonouredAfterRemoved();
    void tst_addDockWidgetAsTabToDockWidget();
    void tst_close();
    void tst_closeTabHidesDockWidget();
    void tst_propagateSizeHonoursMinSize();
    void tst_nonDockable();
    void tst_floatingAction();
    void tst_flagDoubleClick();
    void tst_constraintsPropagateUp();
    void tst_constraintsAfterPlaceholder();
    void tst_addToSmallMainWindow1();
    void tst_addToSmallMainWindow2();
    void tst_addToSmallMainWindow3();
    void tst_addToSmallMainWindow4();
    void tst_addToSmallMainWindow5();
    void tst_positionWhenShown();
    void tst_28NestedWidgets();
    void tst_28NestedWidgets_data();
    void tst_dragBySingleTab();
    void tst_dragByTabBar();
    void tst_dragByTabBar_data();
    void tst_titleBarFocusedWhenTabsChange();
    void tst_dock2FloatingWidgetsTabbed();
    void tst_deleteOnClose();
    void tst_toggleAction();
    void tst_redocksToPreviousTabIndex();
    void tst_toggleTabbed();
    void tst_toggleTabbed2();
    void tst_resizePropagatesEvenly();

    void tst_addMDIDockWidget();
    void tst_redockToMDIRestoresPosition();
    void tst_persistentCentralWidget();

#ifdef KDDOCKWIDGETS_QTWIDGETS
    // TODO: Port these to QtQuick

    void tst_mainWindowAlwaysHasCentralWidget();
    void tst_dockableMainWindows();

    // But these are fine to be widget only:
    void tst_tabsNotClickable();
    void tst_embeddedMainWindow();
    void tst_restoreEmbeddedMainWindow();
    void tst_negativeAnchorPositionWhenEmbedded();
    void tst_negativeAnchorPositionWhenEmbedded_data();
    void tst_closeRemovesFromSideBar();
    void tst_restoreSideBar();
    void tst_toggleActionOnSideBar();
    void tst_deleteOnCloseWhenOnSideBar();
    void tst_sidebarOverlayGetsHiddenOnClick();
    void tst_floatRemovesFromSideBar();
    void tst_overlayedGeometryIsSaved();
    void tst_overlayCrash();

    // And fix these
    void tst_floatingWindowDeleted();
    void tst_addToSmallMainWindow6();
    void tst_minSizeChanges();
    void tst_maxSizePropagates();
    void tst_maxSizePropagates2();
    void tst_maxSizedFloatingWindow();
    void tst_restoreResizesLayout();
    void tst_restoreNonRelativeFloatingWindowGeometry();
    void tst_maxSizeHonouredWhenDropped();
    void tst_fixedSizePolicy();
    void tst_maxSizeHonouredWhenAnotherDropped();
    void tst_addToHiddenMainWindow();
    void tst_maximumSizePolicy();
    void tst_complex();
#else
    void tst_hoverShowsDropIndicators();
#endif
};
