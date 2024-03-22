/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief The GUI counterpart of Frame.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "FrameQuick_p.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"
#include "TabWidgetQuick_p.h"
#include "DockWidgetQuick.h"
#include "../DockWidgetBase_p.h"
#include "../WidgetResizeHandler_p.h"

#include <QDebug>

using namespace KDDockWidgets;

FrameQuick::FrameQuick(QWidgetAdapter *parent, FrameOptions options, int userType)
    : Frame(parent, options, userType)
{
    connect(m_tabWidget->asWidget(), SIGNAL(countChanged()), /// clazy:exclude=old-style-connect
            this, SLOT(updateConstriants()));

    connect(m_tabWidget->asWidget(), SIGNAL(currentDockWidgetChanged(KDDockWidgets::DockWidgetBase*)), /// clazy:exclude=old-style-connect
            this, SIGNAL(currentDockWidgetChanged(KDDockWidgets::DockWidgetBase*)));

    connect(this, &QWidgetAdapter::geometryUpdated, this, &Frame::layoutInvalidated);

    connect(this, &QWidgetAdapter::itemGeometryChanged, this, [this] {
        for (auto dw : dockWidgets()) {
            Q_EMIT static_cast<DockWidgetQuick *>(dw)->frameGeometryChanged(QWidgetAdapter::geometry());
        }
    });

    QQmlComponent component(Config::self().qmlEngine(),
                            Config::self().frameworkWidgetFactory()->frameFilename());

    m_visualItem = static_cast<QQuickItem *>(component.create());

    if (!m_visualItem) {
        qWarning() << Q_FUNC_INFO << "Failed to create item" << component.errorString();
        return;
    }

    m_visualItem->setProperty("frameCpp", QVariant::fromValue(this));
    m_visualItem->setParentItem(this);
    m_visualItem->setParent(this);
}

FrameQuick::~FrameQuick()
{
    {
        const DockWidgetBase::List docks = dockWidgets();

        // The QML item must be deleted with deleteLater(), has we might be currently with its mouse
        // handler in the stack. QML doesn't support it being deleted in that case.
        // So unparent it and deleteLater().
        m_visualItem->setParent(nullptr);
        m_visualItem->deleteLater();

        qDeleteAll(docks);
    }
}

void FrameQuick::updateConstriants()
{
    onDockWidgetCountChanged();

    // QtQuick doesn't have layouts, so we need to do constraint propagation manually

    setProperty("kddockwidgets_min_size", minimumSize());
    setProperty("kddockwidgets_max_size", maximumSize());

    Q_EMIT layoutInvalidated();
}

void FrameQuick::removeWidget_impl(DockWidgetBase *dw)
{
    if (dw->parent() == m_stackLayout) {
        dw->setParent(nullptr);
    }

    m_tabWidget->removeDockWidget(dw);

    if (m_connections.contains(dw)) {
        disconnect(m_connections.take(dw));
    }
}

int FrameQuick::indexOfDockWidget_impl(const DockWidgetBase *dw)
{
    return m_tabWidget->indexOfDockWidget(dw);
}

int FrameQuick::currentIndex_impl() const
{
    return m_tabWidget->currentIndex();
}

void FrameQuick::setCurrentTabIndex_impl(int index)
{
    setCurrentDockWidget_impl(dockWidgetAt(index));
}

void FrameQuick::setCurrentDockWidget_impl(DockWidgetBase *dw)
{
    m_tabWidget->TabWidget::setCurrentDockWidget(dw);
}

void FrameQuick::insertDockWidget_impl(DockWidgetBase *dw, int index)
{
    QPointer<Frame> oldFrame = dw->d->frame();
    if (m_tabWidget->insertDockWidget(index, dw, {}, {})) {
        dw->setParent(m_stackLayout);

        QMetaObject::Connection conn = connect(dw, &DockWidgetBase::parentChanged, this, [dw, this] {
            if (dw->parent() != m_stackLayout)
                removeWidget_impl(dw);
        });

        m_connections[dw] = conn;
        setCurrentDockWidget_impl(dw);

        if (oldFrame && oldFrame->beingDeletedLater()) {
            // give it a push and delete it immediately.
            // Having too many deleteLater() puts us in an inconsistent state. For example if LayoutSaver::saveState()
            // would to be called while the Frame hadn't been deleted yet it would count with that frame unless hacks.
            // Also the unit-tests are full of waitForDeleted() due to deleteLater.

            // Ideally we would just remove the deleteLater from frame.cpp, but QTabWidget::insertTab()
            // would crash, as it accesses the old tab-widget we're stealing from

            delete oldFrame;
        }
    }
}

DockWidgetBase *FrameQuick::dockWidgetAt_impl(int index) const
{
    return m_tabWidget->dockwidgetAt(index);
}

DockWidgetBase *FrameQuick::currentDockWidget_impl() const
{
    return m_tabWidget->currentDockWidget();
}

void FrameQuick::renameTab(int, const QString &)
{
    // Not needed for QtQuick. Our model reacts to titleChanged()
}


void FrameQuick::changeTabIcon(int index, const QIcon &)
{
    Q_UNUSED(index);
    qDebug() << Q_FUNC_INFO << "Not implemented";
}

void FrameQuick::setStackLayout(QQuickItem *stackLayout)
{
    if (m_stackLayout || !stackLayout) {
        qWarning() << Q_FUNC_INFO << "Shouldn't happen";
        return;
    }

    m_stackLayout = stackLayout;
}

QSize FrameQuick::minimumSize() const
{
    const QSize contentsSize = dockWidgetsMinSize();
    return contentsSize + QSize(0, nonContentsHeight());
}

QSize FrameQuick::maximumSize() const
{
    return Frame::maximumSize();
}

QObject *FrameQuick::tabWidgetObj() const
{
    return m_tabWidget->asWidget();
}

TabWidget *FrameQuick::tabWidget() const
{
    return m_tabWidget;
}

QQuickItem *FrameQuick::visualItem() const
{
    return m_visualItem;
}

int FrameQuick::nonContentsHeight() const
{
    return m_visualItem->property("nonContentsHeight").toInt();
}
