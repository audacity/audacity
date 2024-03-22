/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief The QWidget counter part of TabWidgetWidget. Handles GUI while TabWidget handles state.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "TabWidgetWidget_p.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"
#include "../Frame_p.h"
#include "../TitleBar_p.h"
#include "../DockRegistry_p.h"

#include <QMouseEvent>
#include <QTabBar>
#include <QHBoxLayout>
#include <QAbstractButton>
#include <QMenu>

using namespace KDDockWidgets;

TabWidgetWidget::TabWidgetWidget(Frame *parent)
    : QTabWidget(parent)
    , TabWidget(this, parent)
    , m_tabBar(Config::self().frameworkWidgetFactory()->createTabBar(this))
{
    setTabBar(static_cast<QTabBar *>(m_tabBar->asWidget()));
    setTabsClosable(Config::self().flags() & Config::Flag_TabsHaveCloseButton);

    setContextMenuPolicy(Qt::CustomContextMenu);
    connect(this, &QTabWidget::customContextMenuRequested, this, &TabWidgetWidget::showContextMenu);

    // In case tabs closable is set by the factory, a tabClosedRequested() is emitted when the user presses [x]
    connect(this, &QTabWidget::tabCloseRequested, this, [this](int index) {
        if (DockWidgetBase *dw = dockwidgetAt(index)) {
            if (dw->options() & DockWidgetBase::Option_NotClosable) {
                qWarning() << "QTabWidget::tabCloseRequested: Refusing to close dock widget with Option_NotClosable option. name=" << dw->uniqueName();
            } else {
                dw->close();
            }
        } else {
            qWarning() << "QTabWidget::tabCloseRequested Couldn't find dock widget for index" << index << "; count=" << count();
        }
    });

    connect(this, &QTabWidget::currentChanged, this, [this](int index) {
        onCurrentTabChanged(index);
        Q_EMIT currentTabChanged(index);
        Q_EMIT currentDockWidgetChanged(currentDockWidget());
    });

    if (!QTabWidget::tabBar()->isVisible())
        setFocusProxy(nullptr);

    setupTabBarButtons();
}

TabBar *TabWidgetWidget::tabBar() const
{
    return m_tabBar;
}

int TabWidgetWidget::numDockWidgets() const
{
    return count();
}

void TabWidgetWidget::removeDockWidget(DockWidgetBase *dw)
{
    removeTab(indexOf(dw));
}

int TabWidgetWidget::indexOfDockWidget(const DockWidgetBase *dw) const
{
    return indexOf(const_cast<DockWidgetBase *>(dw));
}

void TabWidgetWidget::mouseDoubleClickEvent(QMouseEvent *ev)
{
    if (onMouseDoubleClick(ev->pos())) {
        ev->accept();
    } else {
        ev->ignore();
    }
}

void TabWidgetWidget::mousePressEvent(QMouseEvent *ev)
{
    QTabWidget::mousePressEvent(ev);

    if ((Config::self().flags() & Config::Flag_TitleBarIsFocusable) && !frame()->isFocused()) {
        // User clicked on the tab widget itself
        frame()->FocusScope::focus(Qt::MouseFocusReason);
    }
}

void TabWidgetWidget::tabInserted(int)
{
    onTabInserted();
}

void TabWidgetWidget::tabRemoved(int)
{
    onTabRemoved();
}

bool TabWidgetWidget::isPositionDraggable(QPoint p) const
{
    if (tabPosition() != QTabWidget::North) {
        qWarning() << Q_FUNC_INFO << "Not implemented yet. Only North is supported";
        return false;
    }

    return p.y() >= 0 && p.y() <= QTabWidget::tabBar()->height();
}

void TabWidgetWidget::setCurrentDockWidget(int index)
{
    setCurrentIndex(index);
}

bool TabWidgetWidget::insertDockWidget(int index, DockWidgetBase *dw,
                                       const QIcon &icon, const QString &title)
{
    insertTab(index, dw, icon, title);
    return true;
}

void TabWidgetWidget::setTabBarAutoHide(bool b)
{
    QTabWidget::setTabBarAutoHide(b);
}

void TabWidgetWidget::renameTab(int index, const QString &text)
{
    setTabText(index, text);
}

void TabWidgetWidget::changeTabIcon(int index, const QIcon &icon)
{
    setTabIcon(index, icon);
}

DockWidgetBase *TabWidgetWidget::dockwidgetAt(int index) const
{
    return qobject_cast<DockWidgetBase *>(widget(index));
}

int TabWidgetWidget::currentIndex() const
{
    return QTabWidget::currentIndex();
}

void TabWidgetWidget::setupTabBarButtons()
{
    if (!(Config::self().flags() & Config::Flag_ShowButtonsOnTabBarIfTitleBarHidden))
        return;

    auto factory = Config::self().frameworkWidgetFactory();
    m_closeButton = factory->createTitleBarButton(this, TitleBarButtonType::Close);
    m_floatButton = factory->createTitleBarButton(this, TitleBarButtonType::Float);

    auto cornerWidget = new QWidget(this);
    cornerWidget->setObjectName(QStringLiteral("Corner Widget"));

    setCornerWidget(cornerWidget, Qt::TopRightCorner);

    m_cornerWidgetLayout = new QHBoxLayout(cornerWidget);

    m_cornerWidgetLayout->addWidget(m_floatButton);
    m_cornerWidgetLayout->addWidget(m_closeButton);

    connect(m_floatButton, &QAbstractButton::clicked, this, [this] {
        TitleBar *tb = frame()->titleBar();
        tb->onFloatClicked();
    });

    connect(m_closeButton, &QAbstractButton::clicked, this, [this] {
        TitleBar *tb = frame()->titleBar();
        tb->onCloseClicked();
    });

    updateMargins();
    connect(DockRegistry::self(), &DockRegistry::windowChangedScreen, this, [this](QWindow *w) {
        if (w == window()->windowHandle())
            updateMargins();
    });
}

void TabWidgetWidget::updateMargins()
{
    const qreal factor = logicalDpiFactor(this);
    m_cornerWidgetLayout->setContentsMargins(QMargins(0, 0, 2, 0) * factor);
    m_cornerWidgetLayout->setSpacing(int(2 * factor));
}

void TabWidgetWidget::showContextMenu(QPoint pos)
{
    if (!(Config::self().flags() & Config::Flag_AllowSwitchingTabsViaMenu))
        return;

    QTabBar *tabBar = QTabWidget::tabBar();
    // We don't want context menu if there is only one tab
    if (tabBar->count() <= 1)
        return;

    // Click on a tab => No menu
    if (tabBar->tabAt(pos) >= 0)
        return;

    // Right click is allowed only on the tabs area
    QRect tabAreaRect = tabBar->rect();
    tabAreaRect.setWidth(this->width());
    if (!tabAreaRect.contains(pos))
        return;

    QMenu menu(this);
    for (int i = 0; i < tabBar->count(); ++i) {
        QAction *action = menu.addAction(tabText(i), this, [this, i] {
            setCurrentIndex(i);
        });
        if (i == currentIndex())
            action->setDisabled(true);
    }
    menu.exec(mapToGlobal(pos));
}
