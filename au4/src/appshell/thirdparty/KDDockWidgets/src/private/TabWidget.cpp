/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief Implements a QTabWidget derived class with support for docking and undocking
 * KDockWidget::DockWidget as tabs .
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "TabWidget_p.h"
#include "Config.h"
#include "DockWidgetBase_p.h"
#include "DragController_p.h"
#include "FloatingWindow_p.h"
#include "Frame_p.h"
#include "FrameworkWidgetFactory.h"
#include "Logging_p.h"
#include "Utils_p.h"
#include "WindowBeingDragged_p.h"

#ifdef QT_WIDGETS_LIB
#include <QTabWidget>
#endif

#include <memory>

using namespace KDDockWidgets;

TabBar::TabBar(QWidgetOrQuick *thisWidget, TabWidget *tabWidget)
    : Draggable(thisWidget)
    , m_tabWidget(tabWidget)
    , m_thisWidget(thisWidget)
{
}

DockWidgetBase *TabBar::dockWidgetAt(int index) const
{
    if (index < 0 || index >= numDockWidgets())
        return nullptr;

    return m_tabWidget->dockwidgetAt(index);
}

DockWidgetBase *TabBar::dockWidgetAt(QPoint localPos) const
{
    return dockWidgetAt(tabAt(localPos));
}

std::unique_ptr<WindowBeingDragged> TabBar::makeWindow()
{
    auto dock = m_lastPressedDockWidget;
    m_lastPressedDockWidget = nullptr; // TODO check if we still have this dock, it might have been deleted

    const bool hideTitleBarWhenTabsVisible = Config::self().flags() & Config::Flag_HideTitleBarWhenTabsVisible;
    const bool alwaysShowTabs = Config::self().flags() & Config::Flag_AlwaysShowTabs;

    if (hideTitleBarWhenTabsVisible) {
        if (dock) {
            if (alwaysShowTabs && hasSingleDockWidget()) {
                // Case #1. User is dragging a tab but there's only 1 tab (and tabs are always visible), so drag everything instead, no detaching happens
                return m_tabWidget->makeWindow();
            }
        } else {
            // Case #2. User is dragging on the QTabBar background, not on an actual tab.
            // As Flag_HideTitleBarWhenTabsVisible is set, we let the user drag through the tab widget background.
            return m_tabWidget->makeWindow();
        }
    } else {
        if (dock && hasSingleDockWidget() && alwaysShowTabs) {
            // Case #3. window with title bar and single tab, no detaching should happen, just use the title bar.
            return {};
        }
    }

    if (!dock)
        return {};

    FloatingWindow *floatingWindow = frame()->detachTab(dock);
    if (!floatingWindow)
        return {};

    auto draggable = KDDockWidgets::usesNativeTitleBar() ? static_cast<Draggable *>(floatingWindow)
                                                         : static_cast<Draggable *>(this);
    return std::unique_ptr<WindowBeingDragged>(new WindowBeingDragged(floatingWindow, draggable));
}

bool TabBar::isWindow() const
{
    // Same semantics as tab widget, no need to duplicate logic
    return m_tabWidget->isWindow();
}

void TabBar::onMousePress(QPoint localPos)
{
    m_lastPressedDockWidget = dockWidgetAt(localPos);
    Frame *frame = this->frame();
    if ((Config::self().flags() & Config::Flag_TitleBarIsFocusable) && !frame->isFocused()) {
        // User clicked on a tab which was already focused
        // A tab changing also counts as a change of scope
        frame->FocusScope::focus(Qt::MouseFocusReason);
    }
}

void TabBar::onMouseDoubleClick(QPoint localPos)
{
    if (DockWidgetBase *dw = dockWidgetAt(localPos))
        dw->setFloating(true);
}

bool TabBar::hasSingleDockWidget() const
{
    return numDockWidgets() == 1;
}

int TabBar::numDockWidgets() const
{
    return m_tabWidget->numDockWidgets();
}

QWidgetOrQuick *TabBar::asWidget() const
{
    return m_thisWidget;
}

DockWidgetBase *TabBar::singleDockWidget() const
{
    return m_tabWidget->singleDockWidget();
}

bool TabBar::isMDI() const
{
    Frame *f = frame();
    return f && f->isMDI();
}

Frame *TabBar::frame() const
{
    return m_tabWidget->frame();
}

TabWidget::TabWidget(QWidgetOrQuick *thisWidget, Frame *frame)
    : Draggable(thisWidget, Config::self().flags() & (Config::Flag_HideTitleBarWhenTabsVisible | Config::Flag_AlwaysShowTabs))
    , m_frame(frame)
    , m_thisWidget(thisWidget)
{
}

void TabWidget::setCurrentDockWidget(DockWidgetBase *dw)
{
    setCurrentDockWidget(indexOfDockWidget(dw));
}

DockWidgetBase *TabWidget::currentDockWidget() const
{
    return dockwidgetAt(currentIndex());
}

void TabWidget::addDockWidget(DockWidgetBase *dock)
{
    insertDockWidget(dock, numDockWidgets());
}

bool TabWidget::insertDockWidget(DockWidgetBase *dock, int index)
{
    Q_ASSERT(dock);
    qCDebug(addwidget) << Q_FUNC_INFO << dock << "; count before=" << numDockWidgets();

    if (index < 0)
        index = 0;
    if (index > numDockWidgets())
        index = numDockWidgets();

    if (contains(dock)) {
        qWarning() << Q_FUNC_INFO << "Refusing to add already existing widget";
        return false;
    }

    QPointer<Frame> oldFrame = dock->d->frame();
    insertDockWidget(index, dock, dock->icon(DockWidgetBase::IconPlace::TabBar), dock->title());
    setCurrentDockWidget(index);

    if (oldFrame && oldFrame->beingDeletedLater()) {
        // give it a push and delete it immediately.
        // Having too many deleteLater() puts us in an inconsistent state. For example if LayoutSaver::saveState()
        // would to be called while the Frame hadn't been deleted yet it would count with that frame unless hacks.
        // Also the unit-tests are full of waitForDeleted() due to deleteLater.

        // Ideally we would just remove the deleteLater from frame.cpp, but QTabWidget::insertTab()
        // would crash, as it accesses the old tab-widget we're stealing from

        delete oldFrame;
    }

    return true;
}

bool TabWidget::contains(DockWidgetBase *dw) const
{
    return indexOfDockWidget(dw) != -1;
}

QWidgetOrQuick *TabWidget::asWidget() const
{
    return m_thisWidget;
}

Frame *TabWidget::frame() const
{
    return m_frame;
}

std::unique_ptr<WindowBeingDragged> TabWidget::makeWindow()
{
    // This is called when using Flag_HideTitleBarWhenTabsVisible
    // For detaching individual tabs, TabBar::makeWindow() is called.
    if (auto floatingWindow = qobject_cast<FloatingWindow *>(asWidget()->window())) {
        if (floatingWindow->hasSingleFrame()) {
            // We're already in a floating window, and it only has 1 dock widget.
            // So there's no detachment to be made, we just move the window.
            return std::unique_ptr<WindowBeingDragged>(new WindowBeingDragged(floatingWindow, this));
        }
    }

    QRect r = m_frame->QWidgetAdapter::geometry();

    const QPoint globalPoint = m_thisWidget->mapToGlobal(QPoint(0, 0));

    auto floatingWindow = Config::self().frameworkWidgetFactory()->createFloatingWindow(m_frame);
    r.moveTopLeft(globalPoint);
    floatingWindow->setSuggestedGeometry(r, SuggestedGeometryHint_GeometryIsFromDocked);
    floatingWindow->show();

    return std::unique_ptr<WindowBeingDragged>(new WindowBeingDragged(floatingWindow, this));
}

bool TabWidget::isWindow() const
{
    if (auto floatingWindow = qobject_cast<FloatingWindow *>(asWidget()->window())) {
        // Case of dragging via the tab widget when the title bar is hidden
        return floatingWindow->hasSingleFrame();
    }

    return false;
}

DockWidgetBase *TabWidget::singleDockWidget() const
{
    if (m_frame->hasSingleDockWidget())
        return m_frame->dockWidgets().first();

    return nullptr;
}

bool TabWidget::isMDI() const
{
    return m_frame && m_frame->isMDI();
}

void TabWidget::onTabInserted()
{
    m_frame->onDockWidgetCountChanged();
}

void TabWidget::onTabRemoved()
{
    m_frame->onDockWidgetCountChanged();
}

void TabWidget::onCurrentTabChanged(int index)
{
    Q_UNUSED(index);
}

bool TabWidget::onMouseDoubleClick(QPoint localPos)
{
    // User clicked the empty space of the tab widget and we don't have title bar
    // We float the entire frame.

    if (!(Config::self().flags() & Config::Flag_HideTitleBarWhenTabsVisible) || tabBar()->dockWidgetAt(localPos))
        return false;

    Frame *frame = this->frame();

    // When using MainWindowOption_HasCentralFrame. The central frame is never detachable.
    if (frame->isCentralFrame())
        return false;

    if (FloatingWindow *fw = frame->floatingWindow()) {
        if (!fw->hasSingleFrame()) {
            makeWindow();
            return true;
        }
    } else if (frame->isInMainWindow()) {
        makeWindow();
        return true;
    }

    return false;
}
