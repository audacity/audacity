/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "Separator_qwidget.h"
#include "Widget_qwidget.h"
#include "Logging_p.h"
#include "Config.h"

#include <QPainter>
#include <QStyleOption>
#include <QRubberBand>
#include <QMouseEvent>

using namespace Layouting;

SeparatorWidget::SeparatorWidget(Layouting::Widget *parent)
    : QWidget(parent->asQWidget())
    , Separator(parent)
    , Layouting::Widget_qwidget(this)
{
    setMouseTracking(true);
}

void SeparatorWidget::paintEvent(QPaintEvent *ev)
{
    if (KDDockWidgets::Config::self().disabledPaintEvents() & KDDockWidgets::Config::CustomizableWidget_Separator) {
        QWidget::paintEvent(ev);
        return;
    }

    QPainter p(this);

    QStyleOption opt;
    opt.palette = palette();
    opt.rect = QWidget::rect();
    opt.state = QStyle::State_None;
    if (!isVertical())
        opt.state |= QStyle::State_Horizontal;

    if (isEnabled())
        opt.state |= QStyle::State_Enabled;

    QWidget::parentWidget()->style()->drawControl(QStyle::CE_Splitter, &opt, &p, this);
}

void SeparatorWidget::enterEvent(KDDockWidgets::Qt5Qt6Compat::QEnterEvent *)
{
    qCDebug(separators) << Q_FUNC_INFO << this;
    if (isVertical())
        setCursor(Qt::SizeVerCursor);
    else
        setCursor(Qt::SizeHorCursor);
}

void SeparatorWidget::leaveEvent(QEvent *)
{
    setCursor(Qt::ArrowCursor);
}

void SeparatorWidget::mousePressEvent(QMouseEvent *)
{
    onMousePress();
}

void SeparatorWidget::mouseMoveEvent(QMouseEvent *ev)
{
    onMouseMove(mapToParent(ev->pos()));
}

void SeparatorWidget::mouseReleaseEvent(QMouseEvent *)
{
    onMouseReleased();
}

void SeparatorWidget::mouseDoubleClickEvent(QMouseEvent *)
{
    onMouseDoubleClick();
}

Layouting::Widget *SeparatorWidget::createRubberBand(Layouting::Widget *parent)
{
    if (!parent) {
        qWarning() << Q_FUNC_INFO << "Parent is required";
        return nullptr;
    }

    return new Layouting::Widget_qwidget(new RubberBand(parent));
}

Widget *SeparatorWidget::asWidget()
{
    return this;
}

Layouting::RubberBand::RubberBand(Widget *parent)
    : QRubberBand(QRubberBand::Line, parent ? parent->asQWidget() : nullptr)
    , Layouting::Widget_qwidget(this)
{
}
