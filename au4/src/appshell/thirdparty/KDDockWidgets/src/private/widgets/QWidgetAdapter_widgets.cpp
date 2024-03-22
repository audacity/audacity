/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

/**
 * @file
 * @brief A class that is QWidget when building for QtWidgets, and QObject when building for QtQuick.
 *
 * Allows to have the same code base supporting both stacks.
 *
 * @author Sérgio Martins \<sergio.martins@kdab.com\>
 */

#include "QWidgetAdapter.h"
#include "../FloatingWindow_p.h"
#include "../Utils_p.h"
#include "Qt5Qt6Compat_p.h"

#include <QResizeEvent>
#include <QMouseEvent>
#include <QWindow>

#include <QtWidgets/private/qwidget_p.h>

using namespace KDDockWidgets;

QWidgetAdapter::QWidgetAdapter(QWidget *parent, Qt::WindowFlags f)
    : QWidget(parent, f)
{
}

QWidgetAdapter::~QWidgetAdapter()
{
}

FloatingWindow *QWidgetAdapter::floatingWindow() const
{
    if (auto fw = qobject_cast<FloatingWindow *>(window()))
        return fw;

    return nullptr;
}

void QWidgetAdapter::raiseAndActivate()
{
    window()->raise();
    if (!isWayland())
        window()->activateWindow();
}

bool QWidgetAdapter::event(QEvent *e)
{
    if (e->type() == QEvent::LayoutRequest)
        onLayoutRequest();

    return QWidget::event(e);
}

void QWidgetAdapter::resizeEvent(QResizeEvent *ev)
{
    if (!onResize(ev->size()))
        QWidget::resizeEvent(ev);
}

void QWidgetAdapter::mousePressEvent(QMouseEvent *)
{
    onMousePress();
}

void QWidgetAdapter::mouseMoveEvent(QMouseEvent *ev)
{
    onMouseMove(Qt5Qt6Compat::eventGlobalPos(ev));
}

void QWidgetAdapter::mouseReleaseEvent(QMouseEvent *)
{
    onMouseRelease();
}

void QWidgetAdapter::closeEvent(QCloseEvent *e)
{
    onCloseEvent(e);
}

void QWidgetAdapter::setFlag(Qt::WindowType f, bool on)
{
    QWidget::setWindowFlag(f, on);
}

void QWidgetAdapter::setSize(QSize size)
{
    QRect geo = geometry();
    geo.setSize(size);
    setGeometry(geo);
}

bool QWidgetAdapter::onResize(QSize)
{
    return false;
}
void QWidgetAdapter::onLayoutRequest()
{
}

void QWidgetAdapter::onMousePress()
{
}
void QWidgetAdapter::onMouseMove(QPoint)
{
}
void QWidgetAdapter::onMouseRelease()
{
}

void QWidgetAdapter::onCloseEvent(QCloseEvent *)
{
}

QWidget *KDDockWidgets::Private::widgetForWindow(QWindow *window)
{
    if (!window)
        return nullptr;

    return window->property("kddockwidgets_qwidget").value<QWidget *>();
}

void QWidgetAdapter::setNormalGeometry(QRect geo)
{
    QWidgetPrivate *priv = QWidgetPrivate::get(this);
    if (priv->extra && priv->extra->topextra) {
        priv->topData()->normalGeometry = geo;
    } else {
        qWarning() << Q_FUNC_INFO << "Failing to set normal geometry";
    }
}

LayoutGuestWidget::~LayoutGuestWidget() = default;
