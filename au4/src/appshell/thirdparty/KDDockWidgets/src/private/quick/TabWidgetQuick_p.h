/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief The QQuickItem counter part of TabWidgetQuick. Handles GUI while TabWidget handles state.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#ifndef KDTABWIDGETQUICK_P_H
#define KDTABWIDGETQUICK_P_H

#include "../TabWidget_p.h"
#include "QWidgetAdapter_quick_p.h"

#include <QQuickItem>
#include <QAbstractListModel>

namespace KDDockWidgets {

class Frame;
class TabBar;
class DockWidgetModel;

class DOCKS_EXPORT TabWidgetQuick
    : public QWidgetAdapter,
      public TabWidget
{
    Q_OBJECT
    Q_PROPERTY(DockWidgetModel *dockWidgetModel READ dockWidgetModel CONSTANT)
    Q_PROPERTY(QObject *tabBar READ tabBarObj CONSTANT)

public:
    explicit TabWidgetQuick(Frame *parent);

    TabBar *tabBar() const override;

    int numDockWidgets() const override;
    void removeDockWidget(DockWidgetBase *) override;
    int indexOfDockWidget(const DockWidgetBase *) const override;
    DockWidgetModel *dockWidgetModel() const;
    DockWidgetBase *dockwidgetAt(int index) const override;
    int currentIndex() const override;
    bool insertDockWidget(int index, DockWidgetBase *, const QIcon &, const QString &title) override;
    Q_INVOKABLE void setCurrentDockWidget(int index) override;

    /// @brief Returns the tab bar as a QObject for QML.
    /// As the base class is not a QObject.
    QObject *tabBarObj() const;

Q_SIGNALS:
    void currentTabChanged(int index) override;
    void currentDockWidgetChanged(KDDockWidgets::DockWidgetBase *dw) override;
    void countChanged() override;

protected:
    bool isPositionDraggable(QPoint p) const override;
    void setTabBarAutoHide(bool) override;
    void renameTab(int index, const QString &) override;
    void changeTabIcon(int index, const QIcon &) override;

private:
    Q_DISABLE_COPY(TabWidgetQuick)
    DockWidgetModel *const m_dockWidgetModel;
    TabBar *const m_tabBar;
    DockWidgetBase *m_currentDockWidget = nullptr;
};

class DockWidgetModel : public QAbstractListModel
{
    Q_OBJECT
public:
    enum Role
    {
        Role_Title = Qt::UserRole
    };

    explicit DockWidgetModel(QObject *parent);
    int count() const;
    int rowCount(const QModelIndex &parent) const override;
    QVariant data(const QModelIndex &index, int role) const override;
    DockWidgetBase *dockWidgetAt(int index) const;
    void remove(DockWidgetBase *);
    int indexOf(const DockWidgetBase *);
    bool insert(DockWidgetBase *dw, int index);
    bool contains(DockWidgetBase *dw) const;

protected:
    QHash<int, QByteArray> roleNames() const override;
Q_SIGNALS:
    void countChanged();

private:
    void emitDataChangedFor(DockWidgetBase *);
    DockWidgetBase::List m_dockWidgets;
    QHash<DockWidgetBase *, QVector<QMetaObject::Connection>> m_connections; // To make it easy to disconnect from lambdas
    bool m_removeGuard = false;
};

}

#endif
