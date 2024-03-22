/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "dropcontroller.h"

#include "../idockwindow.h"
#include "../dockcentralview.h"
#include "../dockingholderview.h"
#include "../dockpageview.h"
#include "../dockpanelview.h"
#include "../docktoolbarview.h"

#include "globaltypes.h"
#include "log.h"

#include "thirdparty/KDDockWidgets/src/DockWidgetQuick.h"
#include "thirdparty/KDDockWidgets/src/MainWindowBase.h"
#include "thirdparty/KDDockWidgets/src/private/DockRegistry_p.h"
#include "thirdparty/KDDockWidgets/src/private/DragController_p.h"
#include "thirdparty/KDDockWidgets/src/private/DropAreaWithCentralFrame_p.h"

using KDDropLocation = KDDockWidgets::DropIndicatorOverlayInterface::DropLocation;

namespace mu::dock {
static constexpr double MAX_DISTANCE_TO_HOLDER = 50;

static KDDropLocation dropLocationToKDDockLocation(Location location)
{
    switch (location) {
    case Location::Undefined: return KDDropLocation::DropLocation_None;
    case Location::Left: return KDDropLocation::DropLocation_Left;
    case Location::Right: return KDDropLocation::DropLocation_Right;
    case Location::Center: return KDDropLocation::DropLocation_Center;
    case Location::Top: return KDDropLocation::DropLocation_Top;
    case Location::Bottom: return KDDropLocation::DropLocation_Bottom;
    }

    return KDDropLocation::DropLocation_None;
}

static bool isPointAllowedForDrop(const QPoint& point, const DropDestination& dropDestination)
{
    QRect dropRect = dropDestination.dock->frameGeometry();

    if (!dropRect.contains(point)) {
        return false;
    }

    if (dropDestination.dropDistance == 0) {
        return true;
    }

    if (dropDestination.dropLocation == Location::Left) {
        if (std::abs(dropRect.left() - point.x()) <= dropDestination.dropDistance) {
            return true;
        }
    }

    if (dropDestination.dropLocation == Location::Right) {
        if (std::abs(dropRect.right() - point.x()) <= dropDestination.dropDistance) {
            return true;
        }
    }

    return false;
}
}

using namespace mu::dock;

DropController::DropController(KDDockWidgets::DropArea* dropArea)
    : KDDockWidgets::DropIndicatorOverlayInterface(dropArea)
{
    KDDockWidgets::DragController::instance()->setResolveDropAreaFunc([](const QPoint& globalPos) -> KDDockWidgets::DropArea* {
        for (auto mainWindow : KDDockWidgets::DockRegistry::self()->mainwindows()) {
            if (mainWindow->windowGeometry().contains(globalPos)) {
                return mainWindow->dropArea();
            }
        }

        return nullptr;
    });
}

KDDropLocation DropController::hover_impl(QPoint globalPos)
{
    DockBase* draggedDock = this->draggedDock();
    if (!draggedDock) {
        return DropLocation_None;
    }

    QPoint hoveredLocalPos = dockWindow()->asItem().mapFromGlobal(globalPos).toPoint();
    DropDestination dropDestination = resolveDropDestination(draggedDock, hoveredLocalPos);

    if (auto toolBar = dynamic_cast<DockToolBarView*>(draggedDock)) {
        updateToolBarOrientation(toolBar, dropDestination);
    }

    setCurrentDropDestination(draggedDock, dropDestination);
    setCurrentDropLocation(dropLocationToKDDockLocation(m_currentDropDestination.dropLocation));

    if (m_currentDropDestination.isValid()) {
        setHoveredFrame(m_currentDropDestination.dock->dockWidget()->frame());
    }

    return currentDropLocation();
}

void DropController::updateVisibility()
{
    auto resetDropLocation = [this]() {
        setCurrentDropLocation(DropLocation_None);
        endHover();
    };

    auto draggedDock = this->draggedDock();

    if (!draggedDock) {
        resetDropLocation();
        return;
    }

    if (!isHovered()) {
        if (auto toolBar = dynamic_cast<DockToolBarView*>(draggedDock)) {
            updateToolBarOrientation(toolBar);
        }

        resetDropLocation();
    }
}

QPoint DropController::posForIndicator(KDDropLocation) const
{
    return QPoint();
}

void DropController::endHover()
{
    if (!m_currentDropDestination.isValid()) {
        return;
    }

    m_currentDropDestination.dock->hideHighlighting();

    if (m_currentDropDestination.dock->type() == DockType::DockingHolder) {
        m_currentDropDestination.dock->close();
    }

    m_currentDropDestination.clear();
}

bool DropController::isMouseOverDock(const QPoint& mouseLocalPos, const DockBase* dock) const
{
    QRect geometry = dock ? dock->frameGeometry() : QRect();
    return geometry.contains(mouseLocalPos);
}

void DropController::updateToolBarOrientation(DockToolBarView* draggedToolBar, const DropDestination& dropDestination)
{
    IF_ASSERT_FAILED(draggedToolBar) {
        return;
    }

    mu::Orientation orientation = mu::Orientation::Horizontal;

    if (!dropDestination.isValid()) {
        draggedToolBar->setOrientation(static_cast<Qt::Orientation>(orientation));
        return;
    }

    switch (dropDestination.dock->location()) {
    case Location::Left:
    case Location::Right:
        orientation = mu::Orientation::Vertical;
        break;
    case Location::Top:
    case Location::Bottom:
        orientation = mu::Orientation::Horizontal;
        break;
    case Location::Center:
    case Location::Undefined:
        break;
    }

    draggedToolBar->setOrientation(static_cast<Qt::Orientation>(orientation));
}

void DropController::setCurrentDropDestination(const DockBase* draggedDock, const DropDestination& dropDestination)
{
    if (m_currentDropDestination == dropDestination) {
        return;
    }

    endHover();

    m_currentDropDestination = dropDestination;

    if (!m_currentDropDestination.isValid()) {
        return;
    }

    auto showHighlighting = [this, draggedDock]() {
        QRect highlightingRect = resolveHighlightingRect(draggedDock, m_currentDropDestination);
        m_currentDropDestination.dock->showHighlighting(highlightingRect);
    };

    if (m_currentDropDestination.dock->type() != DockType::DockingHolder) {
        showHighlighting();
        return;
    }

    switch (m_currentDropDestination.dock->location()) {
    case Location::Left:
    case Location::Right:
        m_currentDropDestination.dock->setMinimumWidth(draggedDock->minimumWidth());
        break;
    case Location::Top:
    case Location::Bottom:
        m_currentDropDestination.dock->setMinimumHeight(draggedDock->minimumHeight());
        break;
    case Location::Center:
    case Location::Undefined:
        break;
    }

    m_currentDropDestination.dock->open();
    showHighlighting();
    m_currentDropDestination.dock->init();
}

DropDestination DropController::resolveDropDestination(const DockBase* draggedDock, const QPoint& localPos) const
{
    if (draggedDock->type() == DockType::Panel) {
        DropDestination destination;

        destination.dock = resolvePanelForDrop(dynamic_cast<const DockPanelView*>(draggedDock), localPos);
        destination.dropLocation = resolveDropLocation(destination.dock, localPos);

        if (destination.isValid()) {
            return destination;
        }
    }

    const DockingHolderView* holder = resolveDockingHolder(draggedDock->type(), localPos);
    QList<DropDestination> destinations = draggedDock->dropDestinations();

    for (const DropDestination& destination : destinations) {
        if (holder == destination.dock) {
            return destination;
        }

        if (isPointAllowedForDrop(localPos, destination)) {
            return destination;
        }
    }

    return DropDestination();
}

DockingHolderView* DropController::resolveDockingHolder(DockType draggedDockType, const QPoint& localPos) const
{
    if (!dockWindow()->asItem().contains(localPos)) {
        return nullptr;
    }

    QRect centralGeometry = currentPage()->centralDock()->frameGeometry();

    // TODO: Need to take any panels docked at top into account
    if (localPos.y() <= centralGeometry.top() + MAX_DISTANCE_TO_HOLDER) {
        return currentPage()->holder(draggedDockType, Location::Top);
    }

    if (localPos.y() >= centralGeometry.bottom() - MAX_DISTANCE_TO_HOLDER) {
        return currentPage()->holder(draggedDockType, Location::Bottom);
    }

    if (localPos.x() <= MAX_DISTANCE_TO_HOLDER) {
        return currentPage()->holder(draggedDockType, Location::Left);
    }

    if (localPos.x() >= dockWindow()->asItem().boundingRect().right() - MAX_DISTANCE_TO_HOLDER) {
        return currentPage()->holder(draggedDockType, Location::Right);
    }

    return nullptr;
}

DockPanelView* DropController::resolvePanelForDrop(const DockPanelView* panel, const QPoint& localPos) const
{
    QList<DockPanelView*> panels = currentPage()->possiblePanelsForTab(panel);

    for (DockPanelView* p : panels) {
        if (isMouseOverDock(localPos, p)) {
            return p;
        }
    }

    return nullptr;
}

Location DropController::resolveDropLocation(const DockBase* hoveredDock, const QPoint& localPos) const
{
    if (!hoveredDock) {
        return Location::Undefined;
    }

    QRect geometry = hoveredDock->frameGeometry();
    Location dockLocation = hoveredDock->location();

    qreal frameEnd = geometry.bottom();
    qreal mousePos = localPos.y();
    Location beginDropLocation = Location::Top;
    Location endDropLocation = Location::Bottom;

    if (dockLocation == Location::Top || dockLocation == Location::Bottom) {
        mousePos = localPos.x();
        frameEnd = geometry.right();
        beginDropLocation = Location::Left;
        endDropLocation = Location::Right;
    }

    if (mousePos <= frameEnd / 3) {
        return beginDropLocation;
    }

    if (mousePos <= frameEnd / 1.5) {
        return Location::Center;
    }

    if (mousePos <= frameEnd) {
        return endDropLocation;
    }

    return Location::Undefined;
}

QRect DropController::resolveHighlightingRect(const DockBase* draggedDock, const DropDestination& destination) const
{
    if (!destination.isValid()) {
        return QRect();
    }

    QRect frameGeometry = destination.dock->frameGeometry();
    int frameWidth = frameGeometry.width();
    int frameHeight = frameGeometry.height();
    QRect fullFrameHighlightingRect = QRect(0, 0, frameWidth, frameHeight);

    if (destination.dock->type() == DockType::DockingHolder) {
        return fullFrameHighlightingRect;
    }

    if (destination.dock->type() == DockType::Central) {
        int draggedDockWidth = draggedDock->frameGeometry().width();

        if (destination.dropLocation == Location::Left) {
            return QRect(0, 0, draggedDockWidth, frameHeight);
        }

        if (destination.dropLocation == Location::Right) {
            return QRect(frameWidth - draggedDockWidth, 0, draggedDockWidth, frameHeight);
        }
    }

    switch (destination.dropLocation) {
    case Location::Top:
        return QRect(0, 0, frameWidth, frameHeight / 2);
    case Location::Bottom:
        return QRect(0, frameHeight / 2, frameWidth, frameHeight / 2);
    case Location::Left:
        return QRect(0, 0, frameWidth / 2, frameHeight);
    case Location::Right:
        return QRect(frameWidth / 2, 0, frameWidth / 2, frameHeight);
    case Location::Center:
        return fullFrameHighlightingRect;
    case Location::Undefined:
        break;
    }

    return QRect();
}

IDockWindow* DropController::dockWindow() const
{
    return dockWindowProvider()->window();
}

DockPageView* DropController::currentPage() const
{
    return dockWindow() ? dockWindow()->currentPage() : nullptr;
}

DockBase* DropController::draggedDock() const
{
    auto windowBeingDragged = KDDockWidgets::DragController::instance()->windowBeingDragged();
    if (!windowBeingDragged || windowBeingDragged->dockWidgets().isEmpty()) {
        return nullptr;
    }

    QString dockName = windowBeingDragged->dockWidgets().first()->uniqueName();
    const DockPageView* page = currentPage();

    return page ? page->dockByName(dockName) : nullptr;
}
