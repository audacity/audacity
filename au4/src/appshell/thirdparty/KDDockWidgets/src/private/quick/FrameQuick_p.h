/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/


#ifndef KD_FRAME_QUICK_P_H
#define KD_FRAME_QUICK_P_H

#include "../Frame_p.h"
#include "DockWidgetBase.h"
#include "TabWidgetQuick_p.h"

class QQuickItem;

namespace KDDockWidgets {

class DockWidgetModel;

/**
 * @brief The GUI counterpart of Frame.
 */
class DOCKS_EXPORT FrameQuick : public Frame
{
    Q_OBJECT
    Q_PROPERTY(QObject *tabWidget READ tabWidgetObj CONSTANT)
public:
    explicit FrameQuick(QWidgetAdapter *parent = nullptr, FrameOptions = FrameOption::FrameOption_None,
                        int userType = 0);
    ~FrameQuick() override;

    /// @reimp
    QSize minimumSize() const override;

    /// @reimp
    QSize maximumSize() const override;

    /// @brief returns the tab widget as QObject for usage in QML.
    /// We can't return TabWidget directly as it's not a QObject
    QObject *tabWidgetObj() const;

    /// @brief returns the tab widget
    TabWidget *tabWidget() const;

    /// @brief Returns the QQuickItem which represents this frame on the screen
    QQuickItem *visualItem() const;

protected:
    void removeWidget_impl(DockWidgetBase *) override;
    int indexOfDockWidget_impl(const DockWidgetBase *) override;
    int currentIndex_impl() const override;
    void setCurrentTabIndex_impl(int index) override;
    void setCurrentDockWidget_impl(DockWidgetBase *) override;
    void insertDockWidget_impl(DockWidgetBase *, int index) override;
    DockWidgetBase *dockWidgetAt_impl(int index) const override;
    DockWidgetBase *currentDockWidget_impl() const override;
    void renameTab(int index, const QString &) override;
    void changeTabIcon(int index, const QIcon &) override;

    Q_INVOKABLE void setStackLayout(QQuickItem *);

    int nonContentsHeight() const override;

Q_SIGNALS:
    void tabTitlesChanged();

public Q_SLOTS:
    void updateConstriants();

private:
    QQuickItem *m_stackLayout = nullptr;
    QQuickItem *m_visualItem = nullptr;
    QHash<DockWidgetBase *, QMetaObject::Connection> m_connections; // To make it easy to disconnect from lambdas
};

}

#endif
