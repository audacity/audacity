/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2020-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#include "Separator_quick.h"
#include "Widget_quick.h"
#include "Logging_p.h"
#include "Item_p.h"
#include "Rubberband_quick.h"

#include <QTimer>

using namespace Layouting;

SeparatorQuick::SeparatorQuick(Layouting::Widget *parent)
    : QQuickItem(qobject_cast<QQuickItem *>(parent->asQObject()))
    , Separator(parent)
    , Layouting::Widget_quick(this)
{
    createQQuickItem(QStringLiteral(":/kddockwidgets/multisplitter/private/multisplitter/qml/Separator.qml"), this);

    // Only set on Separator::init(), so single-shot
    QTimer::singleShot(0, this, &SeparatorQuick::isVerticalChanged);
}

bool SeparatorQuick::isVertical() const
{
    return Separator::isVertical();
}

Layouting::Widget *SeparatorQuick::createRubberBand(Layouting::Widget *parent)
{
    if (!parent) {
        qWarning() << Q_FUNC_INFO << "Parent is required";
        return nullptr;
    }

    return new Layouting::Widget_quick(new Layouting::RubberBand(parent));
}

Widget *SeparatorQuick::asWidget()
{
    return this;
}

void SeparatorQuick::onMousePressed()
{
    Separator::onMousePress();
}

void SeparatorQuick::onMouseMoved(QPointF localPos)
{
    const QPointF pos = QQuickItem::mapToItem(parentItem(), localPos);
    Separator::onMouseMove(pos.toPoint());
}

void SeparatorQuick::onMouseReleased()
{
    Separator::onMouseReleased();
}

void SeparatorQuick::onMouseDoubleClicked()
{
    Separator::onMouseDoubleClick();
}
