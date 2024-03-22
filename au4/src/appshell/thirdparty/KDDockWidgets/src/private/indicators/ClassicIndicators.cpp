/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "ClassicIndicators_p.h"
#include "Config.h"
#include "FrameworkWidgetFactory.h"
#include "ClassicIndicatorsWindow_p.h"

#include "private/DropArea_p.h"
#include "private/DragController_p.h"
#include "private/Frame_p.h"
#include "private/Logging_p.h"
#include "private/DockRegistry_p.h"
#include "private/Utils_p.h"

using namespace KDDockWidgets;

static IndicatorWindow *createIndicatorWindow(ClassicIndicators *classicIndicators)
{
    auto window = new IndicatorWindow(classicIndicators);
    window->setObjectName(QStringLiteral("_docks_IndicatorWindow_Overlay"));

    return window;
}

ClassicIndicators::ClassicIndicators(DropArea *dropArea)
    : DropIndicatorOverlayInterface(dropArea) // Is parented on the drop-area, not a toplevel.
    , m_rubberBand(Config::self().frameworkWidgetFactory()->createRubberBand(rubberBandIsTopLevel() ? nullptr : dropArea))
    , m_indicatorWindow(createIndicatorWindow(this))
{
    if (rubberBandIsTopLevel())
        m_rubberBand->setWindowOpacity(0.5);
}

ClassicIndicators::~ClassicIndicators()
{
    delete m_indicatorWindow;
}

DropIndicatorOverlayInterface::DropLocation ClassicIndicators::hover_impl(QPoint globalPos)
{
    return m_indicatorWindow->hover(globalPos);
}

QPoint ClassicIndicators::posForIndicator(DropIndicatorOverlayInterface::DropLocation loc) const
{
    return m_indicatorWindow->posForIndicator(loc);
}

bool ClassicIndicators::innerIndicatorsVisible() const
{
    return m_innerIndicatorsVisible;
}

bool ClassicIndicators::outterIndicatorsVisible() const
{
    return m_outterIndicatorsVisible;
}

bool ClassicIndicators::tabIndicatorVisible() const
{
    return m_tabIndicatorVisible;
}

bool ClassicIndicators::onResize(QSize)
{
    m_indicatorWindow->resize(window()->size());
    return false;
}

void ClassicIndicators::updateVisibility()
{
    if (isHovered()) {
        m_indicatorWindow->updatePositions();
        m_indicatorWindow->setVisible(true);
        updateWindowPosition();
        updateIndicatorsVisibility(true);
        raiseIndicators();
    } else {
        m_rubberBand->setVisible(false);
        m_indicatorWindow->setVisible(false);
        updateIndicatorsVisibility(false);
    }
}

void ClassicIndicators::updateIndicatorsVisibility(bool visible)
{
    const bool isTheOnlyFrame = m_hoveredFrame && m_hoveredFrame->isTheOnlyFrame();

    m_innerIndicatorsVisible = visible && m_hoveredFrame;

    WindowBeingDragged *windowBeingDragged = DragController::instance()->windowBeingDragged();

    // If there's only 1 frame in the layout, the outer indicators are redundant, as they do the same thing as the internal ones.
    // But there might be another window obscuring our target, so it's useful to show the outer indicators in this case
    m_outterIndicatorsVisible = visible && (!isTheOnlyFrame || DockRegistry::self()->isProbablyObscured(m_hoveredFrame->window()->windowHandle(), windowBeingDragged));


    // Only allow to dock to center if the affinities match
    auto tabbingAllowedFunc = Config::self().tabbingAllowedFunc();
    m_tabIndicatorVisible = m_innerIndicatorsVisible && windowBeingDragged && DockRegistry::self()->affinitiesMatch(m_hoveredFrame->affinities(), windowBeingDragged->affinities()) && m_hoveredFrame->isDockable();
    if (m_tabIndicatorVisible && tabbingAllowedFunc) {
        const DockWidgetBase::List source = windowBeingDragged->dockWidgets();
        const DockWidgetBase::List target = m_hoveredFrame->dockWidgets();
        m_tabIndicatorVisible = tabbingAllowedFunc(source, target);
    }

    Q_EMIT innerIndicatorsVisibleChanged();
    Q_EMIT outterIndicatorsVisibleChanged();
    Q_EMIT tabIndicatorVisibleChanged();
}

void ClassicIndicators::raiseIndicators()
{
    m_indicatorWindow->raise();
}

KDDockWidgets::Location locationToMultisplitterLocation(ClassicIndicators::DropLocation location)
{
    switch (location) {
    case DropIndicatorOverlayInterface::DropLocation_Left:
        return KDDockWidgets::Location_OnLeft;
    case DropIndicatorOverlayInterface::DropLocation_Top:
        return KDDockWidgets::Location_OnTop;
    case DropIndicatorOverlayInterface::DropLocation_Right:
        return KDDockWidgets::Location_OnRight;
    case DropIndicatorOverlayInterface::DropLocation_Bottom:
        return KDDockWidgets::Location_OnBottom;
    case DropIndicatorOverlayInterface::DropLocation_OutterLeft:
        return KDDockWidgets::Location_OnLeft;
    case DropIndicatorOverlayInterface::DropLocation_OutterTop:
        return KDDockWidgets::Location_OnTop;
    case DropIndicatorOverlayInterface::DropLocation_OutterRight:
        return KDDockWidgets::Location_OnRight;
    case DropIndicatorOverlayInterface::DropLocation_OutterBottom:
        return KDDockWidgets::Location_OnBottom;
    default:
        return KDDockWidgets::Location_None;
    }
}

void ClassicIndicators::setDropLocation(ClassicIndicators::DropLocation location)
{
    setCurrentDropLocation(location);

    if (location == DropLocation_None) {
        m_rubberBand->setVisible(false);
        return;
    }

    if (location == DropLocation_Center) {
        m_rubberBand->setGeometry(geometryForRubberband(m_hoveredFrame ? m_hoveredFrame->QWidgetAdapter::geometry() : rect()));
        m_rubberBand->setVisible(true);
        if (rubberBandIsTopLevel()) {
            m_rubberBand->raise();
            raiseIndicators();
        }

        return;
    }

    KDDockWidgets::Location multisplitterLocation = locationToMultisplitterLocation(location);
    Frame *relativeToFrame = nullptr;

    switch (location) {
    case DropLocation_Left:
    case DropLocation_Top:
    case DropLocation_Right:
    case DropLocation_Bottom:
        if (!m_hoveredFrame) {
            qWarning() << "ClassicIndicators::setCurrentDropLocation: frame is null. location=" << location
                       << "; isHovered=" << isHovered()
                       << "; dropArea->widgets=" << m_dropArea->items();
            Q_ASSERT(false);
            return;
        }
        relativeToFrame = m_hoveredFrame;
        break;
    case DropLocation_OutterLeft:
    case DropLocation_OutterTop:
    case DropLocation_OutterRight:
    case DropLocation_OutterBottom:
        break;
    default:
        break;
    }

    auto windowBeingDragged = DragController::instance()->windowBeingDragged();

    QRect rect = m_dropArea->rectForDrop(windowBeingDragged, multisplitterLocation,
                                         m_dropArea->itemForFrame(relativeToFrame));

    m_rubberBand->setGeometry(geometryForRubberband(rect));
    m_rubberBand->setVisible(true);
    if (rubberBandIsTopLevel()) {
        m_rubberBand->raise();
        raiseIndicators();
    }
}

void ClassicIndicators::updateWindowPosition()
{
    QRect rect = this->rect();
    if (KDDockWidgets::isWindow(m_indicatorWindow)) {
        // On all non-wayland platforms it's a top-level.
        QPoint pos = mapToGlobal(QPoint(0, 0));
        rect.moveTo(pos);
    }
    m_indicatorWindow->setGeometry(rect);
}

bool ClassicIndicators::rubberBandIsTopLevel() const
{
    return Config::self().internalFlags() & Config::InternalFlag_TopLevelIndicatorRubberBand;
}

QRect ClassicIndicators::geometryForRubberband(QRect localRect) const
{
    if (!rubberBandIsTopLevel())
        return localRect;

    QPoint topLeftLocal = localRect.topLeft();
    QPoint topLeftGlobal = m_dropArea->QWidgetAdapter::mapToGlobal(topLeftLocal);

    localRect.moveTopLeft(topLeftGlobal);

    return localRect;
}
