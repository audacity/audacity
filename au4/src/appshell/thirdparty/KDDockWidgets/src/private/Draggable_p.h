/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_DRAGGABLE_P_H
#define KD_DRAGGABLE_P_H

#include "kddockwidgets/QWidgetAdapter.h"

#include <QVector>

#include <memory>

namespace KDDockWidgets {

class DockWidgetBase;
class FloatingWindow;
class WidgetResizeHandler;
struct WindowBeingDragged;

/**
 * @brief Represents something that can be dragged.
 *
 * Like a tab, a docked title bar, and even a FloatingWindow if it has a native OS title bar
 *
 * \internal
 */
class DOCKS_EXPORT Draggable
{
public:
    typedef QVector<Draggable *> List;

    explicit Draggable(QWidgetOrQuick *thisObject, bool enabled = true);
    virtual ~Draggable();
    QWidgetOrQuick *asWidget() const;

    /**
     * If this draggable is already a window, do nothing.
     * Otherwise it should make it a window. As that's what dragging does.
     */
    virtual std::unique_ptr<WindowBeingDragged> makeWindow() = 0;

    /**
     * @brief Returns whether point @p p is draggable.
     *
     * Because simply inheriting from Draggable doesn't mean you can click anywhere to drag.
     * @param p is the point where the mouse press occurred
     */
    virtual bool isPositionDraggable(QPoint p) const
    {
        Q_UNUSED(p)
        return true;
    }

    /**
     * @brief Returns whether a mouse move can start a drag or not.
     * The default implementation just checks if the delta is bigger than
     * QApplication::startDragDistance().
     */
    virtual bool dragCanStart(QPoint pressPos, QPoint globalPos) const;

    /**
     * @brief Sets a widget resize handler
     */
    void setWidgetResizeHandler(WidgetResizeHandler *w);

    /**
     * @brief If this draggable contains a single dock widget, then it's returned.
     * nullptr otherwise.
     *
     * Example: This draggable is a floating window with only 1 dock widget
     * Example:  This draggable is a title bar with two dock widgets -> nullptr
     */
    virtual DockWidgetBase *singleDockWidget() const = 0;

    ///@brief Returns whether this draggable is a MDI window, being dragged internally within a main window
    virtual bool isMDI() const = 0;

    /**
     * @brief Returns whether this draggable is already a window.
     *
     * If true, means the drag will simply move the existing window, and no undocking/untabbing is involved.
     */
    virtual bool isWindow() const = 0;

    /**
     * @brief Maps the given point in draggable mouse area's coordinate system to
     * the equivalent point in window's coordinate system,
     * and returns the mapped coordinate.
     *
     * TODO: Probably a good idea to override this for TitleBar too. Since titlebar's 0,0
     * is approx equal to the floating window's 0,0 the discrepancy isn't noticeable, but it can
     * be if there's more window margins
     */
    virtual QPoint mapToWindow(QPoint pos) const
    {
        return pos;
    }

private:
    class Private;
    Private *const d;
    Q_DISABLE_COPY(Draggable)
};
}

#endif
