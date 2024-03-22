/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "DropArea_p.h"
#include "Config.h"
#include "DockRegistry_p.h"
#include "DockWidgetBase.h"
#include "DockWidgetBase_p.h"
#include "Draggable_p.h"
#include "DropIndicatorOverlayInterface_p.h"
#include "FloatingWindow_p.h"
#include "Frame_p.h"
#include "FrameworkWidgetFactory.h"
#include "Logging_p.h"
#include "MainWindowBase.h"
#include "Utils_p.h"

// #include "indicators/AnimatedIndicators_p.h"
#include "WindowBeingDragged_p.h"

using namespace KDDockWidgets;

/**
 * @file
 * @brief A MultiSplitter with support for drop indicators when hovering over.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */
DropArea::DropArea(QWidgetOrQuick *parent)
    : MultiSplitter(parent)
    , m_dropIndicatorOverlay(Config::self().frameworkWidgetFactory()->createDropIndicatorOverlay(this))
{
    qCDebug(creation) << "DropArea";
    if (isWayland()) {
#ifdef KDDOCKWIDGETS_QTWIDGETS
        setAcceptDrops(true);
#else
        qWarning() << "Dropping not implement for QtQuick on Wayland yet!";
#endif
    }
}

DropArea::~DropArea()
{
    m_inDestructor = true;
    qCDebug(creation) << "~DropArea";
}

Frame::List DropArea::frames() const
{
    return findChildren<Frame *>(QString(), Qt::FindDirectChildrenOnly);
}

Frame *DropArea::frameContainingPos(QPoint globalPos) const
{
    const Layouting::Item::List &items = this->items();
    for (Layouting::Item *item : items) {
        auto frame = static_cast<Frame *>(item->guestAsQObject());
        if (!frame || !frame->QWidgetAdapter::isVisible()) {
            continue;
        }

        if (frame->containsMouse(globalPos))
            return frame;
    }
    return nullptr;
}

void DropArea::updateFloatingActions()
{
    const Frame::List frames = this->frames();
    for (Frame *frame : frames)
        frame->updateFloatingActions();
}

Layouting::Item *DropArea::centralFrame() const
{
    for (Layouting::Item *item : this->items()) {
        if (auto f = static_cast<Frame *>(item->guestAsQObject())) {
            if (f->isCentralFrame())
                return item;
        }
    }
    return nullptr;
}

void DropArea::addDockWidget(DockWidgetBase *dw, Location location,
                             DockWidgetBase *relativeTo, InitialOption option)
{
    if (!dw || dw == relativeTo || location == Location_None) {
        qWarning() << Q_FUNC_INFO << "Invalid parameters" << dw << relativeTo << location;
        return;
    }

    if ((option.visibility == InitialVisibilityOption::StartHidden) && dw->d->frame() != nullptr) {
        // StartHidden is just to be used at startup, not to moving stuff around
        qWarning() << Q_FUNC_INFO << "Dock widget already exists in the layout";
        return;
    }

    if (!validateAffinity(dw))
        return;

    Frame *frame = nullptr;
    Frame *relativeToFrame = relativeTo ? relativeTo->d->frame() : nullptr;

    dw->d->saveLastFloatingGeometry();

    const bool hadSingleFloatingFrame = hasSingleFloatingFrame();

    // Check if the dock widget already exists in the layout
    if (containsDockWidget(dw)) {
        Frame *oldFrame = dw->d->frame();
        if (oldFrame->hasSingleDockWidget()) {
            Q_ASSERT(oldFrame->containsDockWidget(dw));
            // The frame only has this dock widget, and the frame is already in the layout. So move the frame instead
            frame = oldFrame;
        } else {
            frame = Config::self().frameworkWidgetFactory()->createFrame();
            frame->addWidget(dw);
        }
    } else {
        frame = Config::self().frameworkWidgetFactory()->createFrame();
        frame->addWidget(dw);
    }

    if (option.startsHidden()) {
        addWidget(dw, location, relativeToFrame, option);
    } else {
        addWidget(frame, location, relativeToFrame, option);
    }

    if (hadSingleFloatingFrame && !hasSingleFloatingFrame()) {
        // The dock widgets that already existed in our layout need to have their floatAction() updated
        // otherwise it's still checked. Only the dropped dock widget got updated
        updateFloatingActions();
    }
}

bool DropArea::containsDockWidget(DockWidgetBase *dw) const
{
    return dw->d->frame() && LayoutWidget::containsFrame(dw->d->frame());
}

bool DropArea::hasSingleFloatingFrame() const
{
    const Frame::List frames = this->frames();
    return frames.size() == 1 && frames.first()->isFloating();
}

QStringList DropArea::affinities() const
{
    if (auto mw = mainWindow()) {
        return mw->affinities();
    } else if (auto fw = floatingWindow()) {
        return fw->affinities();
    }

    return {};
}

void DropArea::layoutParentContainerEqually(DockWidgetBase *dw)
{
    Layouting::Item *item = itemForFrame(dw->d->frame());
    if (!item) {
        qWarning() << Q_FUNC_INFO << "Item not found for" << dw << dw->d->frame();
        return;
    }

    layoutEqually(item->parentBoxContainer());
}

DropIndicatorOverlayInterface::DropLocation DropArea::hover(WindowBeingDragged *draggedWindow, QPoint globalPos)
{
    if (Config::self().dropIndicatorsInhibited() || !validateAffinity(draggedWindow))
        return DropIndicatorOverlayInterface::DropLocation_None;

    if (!m_dropIndicatorOverlay) {
        qWarning() << Q_FUNC_INFO << "The frontend is missing a drop indicator overlay";
        return DropIndicatorOverlayInterface::DropLocation_None;
    }

    Frame *frame = frameContainingPos(globalPos); // Frame is nullptr if MainWindowOption_HasCentralFrame isn't set
    m_dropIndicatorOverlay->setWindowBeingDragged(true);
    m_dropIndicatorOverlay->setHoveredFrame(frame);
    return m_dropIndicatorOverlay->hover(globalPos);
}

static bool isOutterLocation(DropIndicatorOverlayInterface::DropLocation location)
{
    switch (location) {
    case DropIndicatorOverlayInterface::DropLocation_OutterLeft:
    case DropIndicatorOverlayInterface::DropLocation_OutterTop:
    case DropIndicatorOverlayInterface::DropLocation_OutterRight:
    case DropIndicatorOverlayInterface::DropLocation_OutterBottom:
        return true;
    default:
        return false;
    }
}

bool DropArea::drop(WindowBeingDragged *droppedWindow, QPoint globalPos)
{
    FloatingWindow *floatingWindow = droppedWindow->floatingWindow();

    if (floatingWindow == window()) {
        qWarning() << "Refusing to drop onto itself"; // Doesn't happen
        return false;
    }

    if (m_dropIndicatorOverlay->currentDropLocation() == DropIndicatorOverlayInterface::DropLocation_None) {
        qCDebug(hovering) << "DropArea::drop: bailing out, drop location = none";
        return false;
    }

    qCDebug(dropping) << "DropArea::drop:" << droppedWindow;

    hover(droppedWindow, globalPos);
    auto droploc = m_dropIndicatorOverlay->currentDropLocation();
    Frame *acceptingFrame = m_dropIndicatorOverlay->hoveredFrame();
    if (!(acceptingFrame || isOutterLocation(droploc))) {
        qWarning() << "DropArea::drop: asserted with frame=" << acceptingFrame
                   << "; Location=" << droploc;
        return false;
    }

    return drop(droppedWindow, acceptingFrame, droploc);
}

bool DropArea::drop(WindowBeingDragged *draggedWindow, Frame *acceptingFrame,
                    DropIndicatorOverlayInterface::DropLocation droploc)
{
    FloatingWindow *droppedWindow = draggedWindow ? draggedWindow->floatingWindow()
                                                  : nullptr;

    if (isWayland() && !droppedWindow) {
        // This is the Wayland special case.
        // With other platforms, when detaching a tab or dock widget we create the FloatingWindow immediately.
        // With Wayland we delay the floating window until we drop it.
        // Ofc, we could just dock the dockwidget without the temporary FloatingWindow, but this way we reuse
        // 99% of the rest of the code, without adding more wayland special cases
        droppedWindow = draggedWindow->draggable()->makeWindow()->floatingWindow();
        if (!droppedWindow) {
            // Doesn't happen
            qWarning() << Q_FUNC_INFO << "Wayland: Expected window" << draggedWindow;
            return false;
        }
    }

    bool result = true;
    const bool needToFocusNewlyDroppedWidgets = Config::self().flags() & Config::Flag_TitleBarIsFocusable;
    const DockWidgetBase::List droppedDockWidgets = needToFocusNewlyDroppedWidgets
        ? droppedWindow->layoutWidget()->dockWidgets()
        : DockWidgetBase::List(); // just so save some memory allocations for the case where this
        // variable isn't used

    switch (droploc) {
    case DropIndicatorOverlayInterface::DropLocation_Left:
    case DropIndicatorOverlayInterface::DropLocation_Top:
    case DropIndicatorOverlayInterface::DropLocation_Bottom:
    case DropIndicatorOverlayInterface::DropLocation_Right:
        result = drop(droppedWindow, DropIndicatorOverlayInterface::multisplitterLocationFor(droploc), acceptingFrame);
        break;
    case DropIndicatorOverlayInterface::DropLocation_OutterLeft:
    case DropIndicatorOverlayInterface::DropLocation_OutterTop:
    case DropIndicatorOverlayInterface::DropLocation_OutterRight:
    case DropIndicatorOverlayInterface::DropLocation_OutterBottom:
        result = drop(droppedWindow, DropIndicatorOverlayInterface::multisplitterLocationFor(droploc), nullptr);
        break;
    case DropIndicatorOverlayInterface::DropLocation_Center:
        qCDebug(hovering) << "Tabbing" << droppedWindow << "into" << acceptingFrame;
        if (!validateAffinity(droppedWindow, acceptingFrame))
            return false;
        acceptingFrame->addWidget(droppedWindow);
        break;

    default:
        qWarning() << "DropArea::drop: Unexpected drop location" << m_dropIndicatorOverlay->currentDropLocation();
        result = false;
        break;
    }

    if (result) {
        // Window receiving the drop gets raised:
        raiseAndActivate();

        if (needToFocusNewlyDroppedWidgets) {
            // Let's also focus the newly dropped dock widget
            if (!droppedDockWidgets.isEmpty()) {
                // If more than 1 was dropped, we only focus the first one
                Frame *frame = droppedDockWidgets.first()->d->frame();
                frame->FocusScope::focus(Qt::MouseFocusReason);
            } else {
                // Doesn't happen.
                qWarning() << Q_FUNC_INFO << "Nothing was dropped?";
            }
        }
    }

    return result;
}

bool DropArea::drop(QWidgetOrQuick *droppedWindow, KDDockWidgets::Location location, Frame *relativeTo)
{
    qCDebug(docking) << "DropArea::addFrame";

    if (auto dock = qobject_cast<DockWidgetBase *>(droppedWindow)) {
        if (!validateAffinity(dock))
            return false;

        auto frame = Config::self().frameworkWidgetFactory()->createFrame();
        frame->addWidget(dock);
        addWidget(frame, location, relativeTo, DefaultSizeMode::FairButFloor);
    } else if (auto floatingWindow = qobject_cast<FloatingWindow *>(droppedWindow)) {
        if (!validateAffinity(floatingWindow))
            return false;

        const bool hadSingleFloatingFrame = hasSingleFloatingFrame();
        addMultiSplitter(floatingWindow->dropArea(), location, relativeTo,
                         DefaultSizeMode::FairButFloor);
        if (hadSingleFloatingFrame != hasSingleFloatingFrame())
            updateFloatingActions();

        floatingWindow->scheduleDeleteLater();
        return true;
    } else {
        qWarning() << "Unknown dropped widget" << droppedWindow;
        return false;
    }

    return true;
}

void DropArea::removeHover()
{
    m_dropIndicatorOverlay->removeHover();
}

template<typename T>
bool DropArea::validateAffinity(T *window, Frame *acceptingFrame) const
{
    if (!DockRegistry::self()->affinitiesMatch(window->affinities(), affinities())) {
        return false;
    }

    if (acceptingFrame) {
        // We're dropping into another frame (as tabbed), so also check the affinity of the frame
        // not only of the main window, which might be more forgiving
        if (!DockRegistry::self()->affinitiesMatch(window->affinities(), acceptingFrame->affinities())) {
            return false;
        }
    }

    return true;
}
