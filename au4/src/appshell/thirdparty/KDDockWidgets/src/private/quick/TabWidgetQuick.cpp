/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "TabWidgetQuick_p.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"

#include "../Frame_p.h"

#include <QDebug>
#include <QScopedValueRollback>

using namespace KDDockWidgets;

TabWidgetQuick::TabWidgetQuick(Frame *parent)
    : QWidgetAdapter(parent)
    , TabWidget(this, parent)
    , m_dockWidgetModel(new DockWidgetModel(this))
    , m_tabBar(Config::self().frameworkWidgetFactory()->createTabBar(this))
{
    connect(m_dockWidgetModel, &DockWidgetModel::countChanged, this,
            [this] {
                if (m_currentDockWidget && indexOfDockWidget(m_currentDockWidget) == -1) {
                    // The current dock widget was removed, set the first one as current
                    if (numDockWidgets() > 0)
                        setCurrentDockWidget(0);
                }

                Q_EMIT countChanged(); });
}

TabBar *TabWidgetQuick::tabBar() const
{
    return m_tabBar;
}

int TabWidgetQuick::numDockWidgets() const
{
    return m_dockWidgetModel->count();
}

void TabWidgetQuick::removeDockWidget(DockWidgetBase *dw)
{
    m_dockWidgetModel->remove(dw);
}

int TabWidgetQuick::indexOfDockWidget(const DockWidgetBase *dw) const
{
    return m_dockWidgetModel->indexOf(dw);
}

bool TabWidgetQuick::isPositionDraggable(QPoint p) const
{
    Q_UNUSED(p);
    return true;
}

void TabWidgetQuick::setCurrentDockWidget(int index)
{
    DockWidgetBase *dw = dockwidgetAt(index);

    if (m_currentDockWidget != dw) {
        m_currentDockWidget = dw;
        Q_EMIT currentDockWidgetChanged(dw);
        Q_EMIT currentTabChanged(index);
    }
}

QObject *TabWidgetQuick::tabBarObj() const
{
    return m_tabBar->asWidget();
}

bool TabWidgetQuick::insertDockWidget(int index, DockWidgetBase *dw, const QIcon &, const QString &title)
{
    Q_UNUSED(title); // todo
    return m_dockWidgetModel->insert(dw, index);
}

void TabWidgetQuick::setTabBarAutoHide(bool)
{
    qWarning() << Q_FUNC_INFO << "Not implemented";
}

void TabWidgetQuick::renameTab(int index, const QString &)
{
    Q_UNUSED(index);
    qWarning() << Q_FUNC_INFO << "Not implemented";
}

void TabWidgetQuick::changeTabIcon(int index, const QIcon &)
{
    Q_UNUSED(index);
    qWarning() << Q_FUNC_INFO << "Not implemented";
}

DockWidgetBase *TabWidgetQuick::dockwidgetAt(int index) const
{
    return m_dockWidgetModel->dockWidgetAt(index);
}

int TabWidgetQuick::currentIndex() const
{
    if (!m_currentDockWidget)
        return -1;

    const int index = indexOfDockWidget(m_currentDockWidget);

    if (index == -1)
        qWarning() << Q_FUNC_INFO << "Unexpected null index for" << m_currentDockWidget << this
                   << "; count=" << m_dockWidgetModel->count();

    return index;
}

DockWidgetModel *TabWidgetQuick::dockWidgetModel() const
{
    return m_dockWidgetModel;
}

DockWidgetModel::DockWidgetModel(QObject *parent)
    : QAbstractListModel(parent)
{
}

int DockWidgetModel::count() const
{
    return m_dockWidgets.size();
}

int DockWidgetModel::rowCount(const QModelIndex &parent) const
{
    return parent.isValid() ? 0 : m_dockWidgets.size();
}

QVariant DockWidgetModel::data(const QModelIndex &index, int role) const
{
    const int row = index.row();
    if (row < 0 || row >= m_dockWidgets.size())
        return {};

    DockWidgetBase *dw = m_dockWidgets.at(row);

    switch (role) {
    case Role_Title:
        return dw->title();
    }

    return {};
}

DockWidgetBase *DockWidgetModel::dockWidgetAt(int index) const
{
    if (index < 0 || index >= m_dockWidgets.size()) {
        // Can happen. Benign.
        return nullptr;
    }

    return m_dockWidgets[index];
}

bool DockWidgetModel::contains(DockWidgetBase *dw) const
{
    return m_dockWidgets.contains(dw);
}

QHash<int, QByteArray> DockWidgetModel::roleNames() const
{
    return { { Role_Title, "title" } };
}

void DockWidgetModel::emitDataChangedFor(DockWidgetBase *dw)
{
    const int row = indexOf(dw);
    if (row == -1) {
        qWarning() << Q_FUNC_INFO << "Couldn't find" << dw;
    } else {
        QModelIndex index = this->index(row, 0);
        Q_EMIT dataChanged(index, index);
    }
}

void DockWidgetModel::remove(DockWidgetBase *dw)
{
    QScopedValueRollback<bool> guard(m_removeGuard, true);
    const int row = indexOf(dw);
    if (row == -1) {
        if (!m_removeGuard) {
            // can happen if there's reentrancy. Some user code reacting
            // to the signals and call remove for whatever reason.
            qWarning() << Q_FUNC_INFO << "Nothing to remove"
                       << static_cast<void *>(dw); // Print address only, as it might be deleted already
        }
    } else {
        const auto connections = m_connections.take(dw);
        for (const QMetaObject::Connection &conn : connections)
            disconnect(conn);

        beginRemoveRows(QModelIndex(), row, row);
        m_dockWidgets.removeOne(dw);
        endRemoveRows();

        Q_EMIT countChanged();
    }
}

int DockWidgetModel::indexOf(const DockWidgetBase *dw)
{
    return m_dockWidgets.indexOf(const_cast<DockWidgetBase *>(dw));
}

bool DockWidgetModel::insert(DockWidgetBase *dw, int index)
{
    if (m_dockWidgets.contains(dw)) {
        qWarning() << Q_FUNC_INFO << "Shouldn't happen";
        return false;
    }

    QMetaObject::Connection conn = connect(dw, &DockWidgetBase::titleChanged, this, [dw, this] {
        emitDataChangedFor(dw);
    });

    QMetaObject::Connection conn2 = connect(dw, &QObject::destroyed, this, [dw, this] {
        remove(dw);
    });

    m_connections[dw] = { conn, conn2 };

    beginInsertRows(QModelIndex(), index, index);
    m_dockWidgets.insert(index, dw);
    endInsertRows();

    Q_EMIT countChanged();
    return true;
}
