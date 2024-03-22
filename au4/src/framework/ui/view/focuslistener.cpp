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

#include "focuslistener.h"

#include <QApplication>

using namespace mu::ui;

FocusListener::FocusListener(QObject* parent)
    : QObject(parent)
{
}

QQuickItem* FocusListener::item() const
{
    return m_item;
}

void FocusListener::setItem(QQuickItem* item)
{
    if (m_item == item) {
        return;
    }

    m_item = item;
    listenFocusChanged();

    emit itemChanged();
}

void FocusListener::listenFocusChanged()
{
    if (!m_item) {
        return;
    }

    connect(m_item, &QQuickItem::activeFocusChanged, this, [this](bool hasActiveFocus) {
        if (hasActiveFocus) {
            qApp->installEventFilter(this);
        } else {
            qApp->removeEventFilter(this);
        }
    });
}

bool FocusListener::eventFilter(QObject* watched, QEvent* event)
{
    if (m_item && watched && m_item->hasActiveFocus() && event->type() == QEvent::MouseButtonPress) {
        QMouseEvent* mouseEvent = static_cast<QMouseEvent*>(event);
        QPointF globalItemPos = m_item->mapToGlobal(QPoint(0, 0));
        QRectF globalItemGeometry = QRect(globalItemPos.x(), globalItemPos.y(), m_item->width(), m_item->height());

#ifdef MU_QT5_COMPAT
        QPointF globalPos = mouseEvent->globalPos();
#else
        QPointF globalPos = mouseEvent->globalPosition();
#endif
        bool needResetFocus = !globalItemGeometry.contains(globalPos);
        if (needResetFocus) {
            m_item->setFocus(false);
        }
    }

    return QObject::eventFilter(watched, event);
}
