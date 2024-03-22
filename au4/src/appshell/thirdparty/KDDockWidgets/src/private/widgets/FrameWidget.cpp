/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief The GUI counterpart of Frame. Inherits Frame and implements paintEvent().
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "FrameWidget_p.h"
#include "../TitleBar_p.h"
#include "../TabWidget_p.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"

#include <QVBoxLayout>
#include <QPainter>
#include <QTableWidget>
#include <QTabBar>

using namespace KDDockWidgets;

///@brief a QVBoxLayout that emits layoutInvalidated so that Item can detect minSize changes
class VBoxLayout : public QVBoxLayout //clazy:exclude=missing-qobject-macro
{
public:
    explicit VBoxLayout(FrameWidget *parent)
        : QVBoxLayout(parent)
        , m_frameWidget(parent)
    {
    }
    ~VBoxLayout() override;

    void invalidate() override
    {
        QVBoxLayout::invalidate();
        Q_EMIT m_frameWidget->layoutInvalidated();
    }

    FrameWidget *const m_frameWidget;
};

VBoxLayout::~VBoxLayout() = default;

FrameWidget::FrameWidget(QWidget *parent, FrameOptions options, int userType)
    : Frame(parent, options, userType)
{
    auto vlayout = new VBoxLayout(this);
    vlayout->setContentsMargins(0, 0, 0, 0);
    vlayout->setSpacing(0);
    vlayout->addWidget(titleBar());
    vlayout->addWidget(m_tabWidget->asWidget());

    m_tabWidget->setTabBarAutoHide(!alwaysShowsTabs());

    if (isOverlayed())
        setAutoFillBackground(true);
}

FrameWidget::~FrameWidget()
{
    m_inDtor = true;
}

void FrameWidget::paintEvent(QPaintEvent *)
{
    if (!isFloating()) {
        QPainter p(this);
        p.setRenderHint(QPainter::Antialiasing);

        const qreal penWidth = 1;
        const qreal halfPenWidth = penWidth / 2;
        const QRectF rectf = QWidget::rect();

        const QColor penColor = isOverlayed() ? QColor(0x666666)
                                              : QColor(184, 184, 184, 184);
        QPen pen(penColor);
        pen.setWidthF(penWidth);
        p.setPen(pen);

        if (isOverlayed()) {
            pen.setJoinStyle(Qt::MiterJoin);
            p.drawRect(rectf.adjusted(halfPenWidth, penWidth, -halfPenWidth, -halfPenWidth));
        } else {
            p.drawRoundedRect(rectf.adjusted(halfPenWidth, halfPenWidth, -halfPenWidth, -halfPenWidth), 2, 2);
        }
    }
}

QSize FrameWidget::maxSizeHint() const
{
    // waste due to QTabWidget margins, tabbar etc.
    const QSize waste = minSize() - dockWidgetsMinSize();
    return waste + biggestDockWidgetMaxSize();
}

int FrameWidget::indexOfDockWidget_impl(const DockWidgetBase *dw)
{
    return m_tabWidget->indexOfDockWidget(dw);
}

void FrameWidget::setCurrentDockWidget_impl(DockWidgetBase *dw)
{
    m_tabWidget->setCurrentDockWidget(dw);
}

int FrameWidget::currentIndex_impl() const
{
    return m_tabWidget->currentIndex();
}

void FrameWidget::insertDockWidget_impl(DockWidgetBase *dw, int index)
{
    m_tabWidget->insertDockWidget(dw, index);
}

void FrameWidget::removeWidget_impl(DockWidgetBase *dw)
{
    m_tabWidget->removeDockWidget(dw);
}

void FrameWidget::setCurrentTabIndex_impl(int index)
{
    m_tabWidget->setCurrentDockWidget(index);
}

DockWidgetBase *FrameWidget::currentDockWidget_impl() const
{
    return m_tabWidget->dockwidgetAt(m_tabWidget->currentIndex());
}

DockWidgetBase *FrameWidget::dockWidgetAt_impl(int index) const
{
    return qobject_cast<DockWidgetBase *>(m_tabWidget->dockwidgetAt(index));
}

QTabBar *FrameWidget::tabBar() const
{
    auto tw = static_cast<QTabWidget *>(m_tabWidget->asWidget());
    return tw->tabBar();
}

QRect FrameWidget::dragRect() const
{
    QRect rect = Frame::dragRect();
    if (rect.isValid())
        return rect;

    if (Config::self().flags() & Config::Flag_HideTitleBarWhenTabsVisible) {
        QTabBar *tabBar = this->tabBar();
        rect.setHeight(tabBar->height());
        rect.setWidth(width() - tabBar->width());
        rect.moveTopLeft(QPoint(tabBar->width(), tabBar->y()));
        rect.moveTopLeft(QWidget::mapToGlobal(rect.topLeft()));
    }

    return rect;
}

void FrameWidget::renameTab(int index, const QString &text)
{
    m_tabWidget->renameTab(index, text);
}

void FrameWidget::changeTabIcon(int index, const QIcon &icon)
{
    m_tabWidget->changeTabIcon(index, icon);
}

int FrameWidget::nonContentsHeight() const
{
    TitleBar *tb = titleBar();
    QWidget *tabBar = this->tabBar();

    return (tb->isVisible() ? tb->height() : 0) + (tabBar->isVisible() ? tabBar->height() : 0);
}
