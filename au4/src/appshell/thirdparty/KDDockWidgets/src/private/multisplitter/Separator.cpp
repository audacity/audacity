/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "Separator_p.h"
#include "Widget.h"
#include "Logging_p.h"
#include "Item_p.h"
#include "MultiSplitterConfig.h"

#include <QGuiApplication>

#ifdef Q_OS_WIN
#include <windows.h>
#endif

using namespace Layouting;

Separator *Separator::s_separatorBeingDragged = nullptr;

/// @brief internal counter just for unit-tests
static int s_numSeparators = 0;

struct Separator::Private
{
    // Only set when anchor is moved through mouse. Side1 if going towards left or top, Side2 otherwise.

    Private(Widget *host)
        : m_hostWidget(host)
    {
    }

    Qt::Orientation orientation = Qt::Horizontal;
    QRect geometry;
    int lazyPosition = 0;
    // SeparatorOptions m_options; TODO: Have a Layouting::Config
    Widget *lazyResizeRubberBand = nullptr;
    ItemBoxContainer *parentContainer = nullptr;
    Layouting::Side lastMoveDirection = Side1;
    const bool usesLazyResize = Config::self().flags() & Config::Flag::LazyResize;
    Widget *const m_hostWidget;
};

Separator::Separator(Widget *hostWidget)
    : d(new Private(hostWidget))
{
    s_numSeparators++;
}

Separator::~Separator()
{
    s_numSeparators--;
    delete d;
    if (isBeingDragged())
        s_separatorBeingDragged = nullptr;
}

bool Separator::isVertical() const
{
    return d->orientation == Qt::Vertical;
}

void Separator::move(int p)
{
    auto w = asWidget();
    if (!w)
        return;

    if (isVertical()) {
        w->move(w->x(), p);
    } else {
        w->move(p, w->y());
    }
}

Qt::Orientation Separator::orientation() const
{
    return d->orientation;
}

void Separator::onMousePress()
{
    s_separatorBeingDragged = this;

    qCDebug(separators) << "Drag started";

    if (d->lazyResizeRubberBand) {
        setLazyPosition(position());
        d->lazyResizeRubberBand->show();
    }
}

void Separator::onMouseDoubleClick()
{
    // a double click means we'll resize the left and right neighbour so that they occupy
    // the same size (or top/bottom, depending on orientation).
    d->parentContainer->requestEqualSize(this);
}

void Separator::onMouseMove(QPoint pos)
{
    if (!isBeingDragged())
        return;

    if (!(qApp->mouseButtons() & Qt::LeftButton)) {
        qCDebug(separators) << Q_FUNC_INFO << "Ignoring spurious mouse event. Someone ate our ReleaseEvent";
        onMouseReleased();
        return;
    }

#ifdef Q_OS_WIN
    // Try harder, Qt can be wrong, if mixed with MFC
    const bool mouseButtonIsReallyDown = (GetKeyState(VK_LBUTTON) & 0x8000) || (GetKeyState(VK_RBUTTON) & 0x8000);
    if (!mouseButtonIsReallyDown) {
        qCDebug(separators) << Q_FUNC_INFO << "Ignoring spurious mouse event. Someone ate our ReleaseEvent";
        onMouseReleased();
        return;
    }
#endif

    const int positionToGoTo = Layouting::pos(pos, d->orientation);
    const int minPos = d->parentContainer->minPosForSeparator_global(this);
    const int maxPos = d->parentContainer->maxPosForSeparator_global(this);

    if ((positionToGoTo > maxPos && position() <= positionToGoTo) || (positionToGoTo < minPos && position() >= positionToGoTo)) {
        // if current pos is 100, and max is 80, we do allow going to 90.
        // Would continue to violate, but only by 10, so allow.

        // On the other hand, if we're already past max-pos, don't make it worse and just
        // return if positionToGoTo is further away from maxPos.

        // Same reasoning for minPos
        return;
    }

    d->lastMoveDirection = positionToGoTo < position() ? Side1
                                                       : (positionToGoTo > position() ? Side2
                                                                                      : Side1); // Last case shouldn't happen though.

    if (d->lazyResizeRubberBand)
        setLazyPosition(positionToGoTo);
    else
        d->parentContainer->requestSeparatorMove(this, positionToGoTo - position());
}

void Separator::onMouseReleased()
{
    if (d->lazyResizeRubberBand) {
        d->lazyResizeRubberBand->hide();
        d->parentContainer->requestSeparatorMove(this, d->lazyPosition - position());
    }

    s_separatorBeingDragged = nullptr;
}

void Separator::setGeometry(QRect r)
{
    if (r != d->geometry) {
        d->geometry = r;
        if (auto w = asWidget()) {
            w->setGeometry(r);
            w->setVisible(true);
        }
    }
}

int Separator::position() const
{
    const QPoint topLeft = d->geometry.topLeft();
    return isVertical() ? topLeft.y() : topLeft.x();
}

QObject *Separator::host() const
{
    return d->m_hostWidget ? d->m_hostWidget->asQObject() : nullptr;
}

void Separator::init(ItemBoxContainer *parentContainer, Qt::Orientation orientation)
{
    if (!parentContainer) {
        qWarning() << Q_FUNC_INFO << "null parentContainer";
        return;
    }

    d->parentContainer = parentContainer;
    d->orientation = orientation;
    d->lazyResizeRubberBand = d->usesLazyResize ? createRubberBand(d->m_hostWidget)
                                                : nullptr;
    asWidget()->setVisible(true);
}

ItemBoxContainer *Separator::parentContainer() const
{
    return d->parentContainer;
}

void Separator::setGeometry(int pos, int pos2, int length)
{
    QRect newGeo = d->geometry;
    if (isVertical()) {
        // The separator itself is horizontal
        newGeo.setSize(QSize(length, Item::separatorThickness));
        newGeo.moveTo(pos2, pos);
    } else {
        // The separator itself is vertical
        newGeo.setSize(QSize(Item::separatorThickness, length));
        newGeo.moveTo(pos, pos2);
    }

    setGeometry(newGeo);
}

bool Separator::isResizing()
{
    return s_separatorBeingDragged != nullptr;
}

int Separator::numSeparators()
{
    return s_numSeparators;
}

void Separator::setLazyPosition(int pos)
{
    if (d->lazyPosition != pos) {
        d->lazyPosition = pos;

        QRect geo = asWidget()->geometry();
        if (isVertical()) {
            geo.moveTop(pos);
        } else {
            geo.moveLeft(pos);
        }

        d->lazyResizeRubberBand->setGeometry(geo);
    }
}

bool Separator::isBeingDragged() const
{
    return s_separatorBeingDragged == this;
}
