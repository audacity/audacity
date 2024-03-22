/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "MainWindowInstantiator_p.h"
#include "MainWindowQuick_p.h"
#include "MainWindowMDI.h"

#include "DockWidgetInstantiator_p.h"

#include "../DockRegistry_p.h"

using namespace KDDockWidgets;

MainWindowInstantiator::MainWindowInstantiator()
    : QQuickItem()
{
}

QString MainWindowInstantiator::uniqueName() const
{
    return m_uniqueName;
}

void MainWindowInstantiator::setUniqueName(const QString &name)
{
    if (name != m_uniqueName) {
        m_uniqueName = name;
        Q_EMIT uniqueNameChanged();
    }
}

MainWindowOptions MainWindowInstantiator::options() const
{
    return m_options;
}

void MainWindowInstantiator::setOptions(MainWindowOptions options)
{
    if (m_options != options) {
        m_options = options;
        Q_EMIT optionsChanged();
    }
}

QStringList MainWindowInstantiator::affinities() const
{
    return m_mainWindow ? m_mainWindow->affinities() : QStringList();
}

bool MainWindowInstantiator::isMDI() const
{
    return m_mainWindow && m_mainWindow->isMDI();
}

void MainWindowInstantiator::addDockWidget(DockWidgetBase *dockWidget, Location location,
                                           DockWidgetBase *relativeTo, QSize initialSize,
                                           InitialVisibilityOption option)
{
    if (!m_mainWindow) {
        qWarning() << Q_FUNC_INFO << "No MainWindow created yet";
        return;
    }

    m_mainWindow->addDockWidget(dockWidget, location, relativeTo, { option, initialSize });
}

void MainWindowInstantiator::addDockWidget(DockWidgetInstantiator *dockWidget, Location location,
                                           DockWidgetInstantiator *relativeTo, QSize initialSize,
                                           InitialVisibilityOption option)
{
    addDockWidget(dockWidget ? dockWidget->dockWidget() : nullptr, location,
                  relativeTo ? relativeTo->dockWidget() : nullptr, initialSize, option);
}

void MainWindowInstantiator::layoutEqually()
{
    if (m_mainWindow)
        m_mainWindow->layoutEqually();
}

void MainWindowInstantiator::layoutParentContainerEqually(DockWidgetBase *dw)
{
    if (m_mainWindow)
        m_mainWindow->layoutParentContainerEqually(dw);
}

void MainWindowInstantiator::moveToSideBar(DockWidgetBase *dw)
{
    if (m_mainWindow)
        m_mainWindow->moveToSideBar(dw);
}

void MainWindowInstantiator::moveToSideBar(DockWidgetBase *dw, SideBarLocation loc)
{
    if (m_mainWindow)
        m_mainWindow->moveToSideBar(dw, loc);
}

void MainWindowInstantiator::restoreFromSideBar(DockWidgetBase *dw)
{
    if (m_mainWindow)
        m_mainWindow->restoreFromSideBar(dw);
}

void MainWindowInstantiator::overlayOnSideBar(DockWidgetBase *dw)
{
    if (m_mainWindow)
        m_mainWindow->overlayOnSideBar(dw);
}

void MainWindowInstantiator::toggleOverlayOnSideBar(DockWidgetBase *dw)
{
    if (m_mainWindow)
        m_mainWindow->toggleOverlayOnSideBar(dw);
}

void MainWindowInstantiator::clearSideBarOverlay(bool deleteFrame)
{
    if (m_mainWindow)
        m_mainWindow->clearSideBarOverlay(deleteFrame);
}

SideBar *MainWindowInstantiator::sideBarForDockWidget(const DockWidgetBase *dw) const
{
    return m_mainWindow ? m_mainWindow->sideBarForDockWidget(dw) : nullptr;
}

bool MainWindowInstantiator::sideBarIsVisible(SideBarLocation loc) const
{
    return m_mainWindow && m_mainWindow->sideBarIsVisible(loc);
}

bool MainWindowInstantiator::closeDockWidgets(bool force)
{
    return m_mainWindow && m_mainWindow->closeDockWidgets(force);
}

void MainWindowInstantiator::classBegin()
{
    // Nothing interesting to do here.
}

void MainWindowInstantiator::componentComplete()
{
    if (m_uniqueName.isEmpty()) {
        qWarning() << Q_FUNC_INFO
                   << "Each DockWidget need an unique name. Set the uniqueName property.";
        return;
    }

    if (DockRegistry::self()->containsMainWindow(m_uniqueName)) {
        // MainWindow already exists
        return;
    }

    if (m_uniqueName.isEmpty()) {
        qWarning() << Q_FUNC_INFO << "Name can't be empty";
        return;
    }

    if (m_mainWindow) {
        qWarning() << Q_FUNC_INFO << "Main window is already initialized";
        return;
    }

    const auto mainWindowOptions = MainWindowOptions(m_options);

    if (mainWindowOptions & MainWindowOption_MDI)
        m_mainWindow = new MainWindowMDI(m_uniqueName, this);
    else
        m_mainWindow = new MainWindowQuick(m_uniqueName, mainWindowOptions, this);
}
