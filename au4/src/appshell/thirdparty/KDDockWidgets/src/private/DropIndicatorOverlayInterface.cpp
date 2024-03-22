/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "DropIndicatorOverlayInterface_p.h"

#include "Frame_p.h"
#include "DropArea_p.h"
#include "DockRegistry_p.h"

using namespace KDDockWidgets;

DropIndicatorOverlayInterface::DropIndicatorOverlayInterface(DropArea *dropArea)
    : QWidgetAdapter(dropArea)
    , m_dropArea(dropArea)
{
    setVisible(false);
    setObjectName(QStringLiteral("DropIndicatorOverlayInterface"));

    connect(DockRegistry::self(), &DockRegistry::dropIndicatorsInhibitedChanged, this,
            [this](bool inhibited) {
                if (inhibited)
                    removeHover();

                // if false then simply moving the mouse will make the drop indicators appear again
            });
}

void DropIndicatorOverlayInterface::setWindowBeingDragged(bool is)
{
    if (is == m_draggedWindowIsHovering)
        return;

    m_draggedWindowIsHovering = is;
    if (is) {
        setGeometry(m_dropArea->QWidgetAdapter::rect());
        raise();
    } else {
        setHoveredFrame(nullptr);
    }

    setVisible(is);
    updateVisibility();
}

QRect DropIndicatorOverlayInterface::hoveredFrameRect() const
{
    return m_hoveredFrameRect;
}

void DropIndicatorOverlayInterface::setHoveredFrame(Frame *frame)
{
    if (frame == m_hoveredFrame)
        return;

    if (m_hoveredFrame)
        disconnect(m_hoveredFrame, &QObject::destroyed, this, &DropIndicatorOverlayInterface::onFrameDestroyed);

    m_hoveredFrame = frame;
    if (m_hoveredFrame) {
        connect(frame, &QObject::destroyed, this, &DropIndicatorOverlayInterface::onFrameDestroyed);
        setHoveredFrameRect(m_hoveredFrame->QWidgetAdapter::geometry());
    } else {
        setHoveredFrameRect(QRect());
    }

    updateVisibility();
    Q_EMIT hoveredFrameChanged(m_hoveredFrame);
    onHoveredFrameChanged(m_hoveredFrame);
}

bool DropIndicatorOverlayInterface::isHovered() const
{
    return m_draggedWindowIsHovering;
}

DropIndicatorOverlayInterface::DropLocation DropIndicatorOverlayInterface::currentDropLocation() const
{
    return m_currentDropLocation;
}

KDDockWidgets::Location DropIndicatorOverlayInterface::multisplitterLocationFor(DropIndicatorOverlayInterface::DropLocation dropLoc)
{
    switch (dropLoc) {
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_None:
        return KDDockWidgets::Location_None;
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_Left:
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_OutterLeft:
        return KDDockWidgets::Location_OnLeft;
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_OutterTop:
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_Top:
        return KDDockWidgets::Location_OnTop;
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_OutterRight:
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_Right:
        return KDDockWidgets::Location_OnRight;
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_OutterBottom:
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_Bottom:
        return KDDockWidgets::Location_OnBottom;
    case KDDockWidgets::DropIndicatorOverlayInterface::DropLocation_Center:
        return KDDockWidgets::Location_None;
    }

    return KDDockWidgets::Location_None;
}

void DropIndicatorOverlayInterface::onFrameDestroyed()
{
    setHoveredFrame(nullptr);
}

void DropIndicatorOverlayInterface::onHoveredFrameChanged(Frame *)
{
}

void DropIndicatorOverlayInterface::setCurrentDropLocation(DropIndicatorOverlayInterface::DropLocation location)
{
    if (m_currentDropLocation != location) {
        m_currentDropLocation = location;
        Q_EMIT currentDropLocationChanged();
    }
}

DropIndicatorOverlayInterface::DropLocation DropIndicatorOverlayInterface::hover(QPoint globalPos)
{
    return hover_impl(globalPos);
}

void DropIndicatorOverlayInterface::setHoveredFrameRect(QRect rect)
{
    if (m_hoveredFrameRect != rect) {
        m_hoveredFrameRect = rect;
        Q_EMIT hoveredFrameRectChanged();
    }
}

void DropIndicatorOverlayInterface::removeHover()
{
    setWindowBeingDragged(false);
    setCurrentDropLocation(DropIndicatorOverlayInterface::DropLocation_None);
}
