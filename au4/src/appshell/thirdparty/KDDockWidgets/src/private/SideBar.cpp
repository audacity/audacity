/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "SideBar_p.h"
#include "DockWidgetBase.h"
#include "MainWindowBase.h"

#include <QDebug>

using namespace KDDockWidgets;

SideBar::SideBar(SideBarLocation location, MainWindowBase *parent)
    : QWidgetAdapter(parent)
    , m_mainWindow(parent)
    , m_location(location)
    , m_orientation((location == SideBarLocation::North || location == SideBarLocation::South) ? Qt::Horizontal
                                                                                               : Qt::Vertical)
{
    updateSize();
}

void SideBar::addDockWidget(DockWidgetBase *dw)
{
    if (!dw)
        return;

    if (m_dockWidgets.contains(dw)) {
        qWarning() << Q_FUNC_INFO << "Already contains dock widget" << dw->title();
        return;
    }

    connect(dw, &QObject::destroyed, this, &SideBar::onDockWidgetDestroyed);

    m_dockWidgets << dw;
    addDockWidget_Impl(dw);
    updateSize();
}

void SideBar::removeDockWidget(DockWidgetBase *dw)
{
    if (!m_dockWidgets.contains(dw)) {
        qWarning() << Q_FUNC_INFO << "Doesn't contain dock widget" << dw->title();
        return;
    }

    disconnect(dw, &QObject::destroyed, this, &SideBar::onDockWidgetDestroyed);
    m_dockWidgets.removeOne(dw);
    removeDockWidget_Impl(dw);
    Q_EMIT dw->removedFromSideBar();
    updateSize();
}

bool SideBar::containsDockWidget(DockWidgetBase *dw) const
{
    return m_dockWidgets.contains(dw);
}

void SideBar::onButtonClicked(DockWidgetBase *dw)
{
    toggleOverlay(dw);
}

void SideBar::onDockWidgetDestroyed(QObject *dw)
{
    removeDockWidget(static_cast<DockWidgetBase *>(dw));
}

void SideBar::updateSize()
{
    const int thickness = isEmpty() ? 0 : 30;
    if (isVertical()) {
        setFixedWidth(thickness);
    } else {
        setFixedHeight(thickness);
    }
}

Qt::Orientation SideBar::orientation() const
{
    return m_orientation;
}

bool SideBar::isEmpty() const
{
    return m_dockWidgets.isEmpty();
}

SideBarLocation SideBar::location() const
{
    return m_location;
}

MainWindowBase *SideBar::mainWindow() const
{
    return m_mainWindow;
}

void SideBar::toggleOverlay(DockWidgetBase *dw)
{
    m_mainWindow->toggleOverlayOnSideBar(dw);
}

QStringList SideBar::serialize() const
{
    QStringList ids;
    ids.reserve(m_dockWidgets.size());
    for (DockWidgetBase *dw : m_dockWidgets)
        ids << dw->uniqueName();

    return ids;
}

void SideBar::clear()
{
    for (DockWidgetBase *dw : std::as_const(m_dockWidgets))
        removeDockWidget(dw);
}
