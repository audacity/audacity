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

#include "TabBarWidget_p.h"
#include "Config.h"

#include <QMouseEvent>
#include <QApplication>
#include <QProxyStyle>

// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro

namespace KDDockWidgets {
namespace { // anonymous namespace to silence -Wweak-vtables
class MyProxy : public QProxyStyle
{
public:
    MyProxy()
        : QProxyStyle(qApp->style())
    {
        setParent(qApp);
    }

    int styleHint(QStyle::StyleHint hint, const QStyleOption *option = nullptr,
                  const QWidget *widget = nullptr, QStyleHintReturn *returnData = nullptr) const override
    {
        if (hint == QStyle::SH_Widget_Animation_Duration) {
            // QTabBar has a bug which causes the paint event to dereference a tab which was already removed.
            // Because, after the tab being removed, the d->pressedIndex is only reset after the animation ends.
            // So disable the animation. Crash can be repro by enabling movable tabs, and detaching a tab quickly from
            // a floating window containing two dock widgets. Reproduced on Windows
            return 0;
        }
        return baseStyle()->styleHint(hint, option, widget, returnData);
    }
};
}
}

using namespace KDDockWidgets;

static MyProxy *proxyStyle()
{
    static auto *proxy = new MyProxy;
    return proxy;
}

TabBarWidget::TabBarWidget(TabWidget *parent)
    : QTabBar(parent->asWidget())
    , TabBar(this, parent)
    , m_tabWidget(parent)
{
    setMovable(Config::self().flags() & Config::Flag_AllowReorderTabs);
    setStyle(proxyStyle());
}

int TabBarWidget::tabAt(QPoint localPos) const
{
    return QTabBar::tabAt(localPos);
}

DockWidgetBase *TabBarWidget::currentDockWidget() const
{
    const int index = currentIndex();
    return index == -1 ? nullptr
                       : dockWidgetAt(index);
}

void TabBarWidget::mousePressEvent(QMouseEvent *e)
{
    onMousePress(e->pos());
    QTabBar::mousePressEvent(e);
}

void TabBarWidget::mouseMoveEvent(QMouseEvent *e)
{
    if (count() > 1) {
        // Only allow to re-order tabs if we have more than 1 tab, otherwise it's just weird.
        QTabBar::mouseMoveEvent(e);
    }
}

void TabBarWidget::mouseDoubleClickEvent(QMouseEvent *e)
{
    TabBar::onMouseDoubleClick(e->pos());
}

bool TabBarWidget::dragCanStart(QPoint pressPos, QPoint pos) const
{
    // Here we allow the user to re-order tabs instead of dragging them off.
    // To do that we just return false here, and QTabBar will handle the mouse event, assuming QTabBar::isMovable.

    const bool defaultResult = Draggable::dragCanStart(pressPos, pos);

    if (!defaultResult || !isMovable()) {
        // Nothing more to do. If the drag wouldn't start anyway, return false.
        // And if the tabs aren't movable, just return the default result, which just considers
        // QApplication::startDragDistances
        return defaultResult;
    }

    const int index = tabAt(mapFromGlobal(pos));
    if (index == -1)
        return defaultResult;

    const int deltaX = qAbs(pos.x() - pressPos.x());
    const int deltaY = qAbs(pos.y() - pressPos.y());

    if (deltaY > 5 * QApplication::startDragDistance()) {
        // Moving up or down too much results in a detach. No tab re-ordering allowed.
        return true;
    } else if (deltaY > QApplication::startDragDistance() && deltaX < QApplication::startDragDistance()) {
        // Moved a bit up or down, but not left/right, then detach too.
        // Only if it's going considerably left/right we allow to re-order tabs.
        return true;
    }

    return false;
}

bool TabBarWidget::event(QEvent *ev)
{
    // Qt has a bug in QWidgetPrivate::deepestFocusProxy(), it doesn't honour visibility
    // of the focus scope. Once an hidden widget is focused the chain is broken and tab
    // stops working (#180)

    auto parent = parentWidget();
    if (!parent)
        return QTabBar::event(ev);

    const bool result = QTabBar::event(ev);

    if (ev->type() == QEvent::Show) {
        parent->setFocusProxy(this);
    } else if (ev->type() == QEvent::Hide) {
        parent->setFocusProxy(nullptr);
    }

    return result;
}

QString TabBarWidget::text(int index) const
{
    return tabText(index);
}

QRect TabBarWidget::rectForTab(int index) const
{
    return QTabBar::tabRect(index);
}

void TabBarWidget::moveTabTo(int from, int to)
{
    moveTab(from, to);
}
