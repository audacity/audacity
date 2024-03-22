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

#include "iconview.h"

#include <QPainter>

using namespace mu::uicomponents;

IconView::IconView(QQuickItem* parent)
    : QuickPaintedView(parent)
{
}

QVariant IconView::icon() const
{
    return QVariant::fromValue(m_icon);
}

void IconView::setIcon(QVariant v)
{
    if (v.canConvert<QIcon>()) {
        m_icon = v.value<QIcon>();
    } else if (v.canConvert<QColor>()) {
        m_color = v.value<QColor>();
        m_icon = QIcon();
    } else if (v.canConvert<QPixmap>()) {
        m_icon = QIcon(v.value<QPixmap>());
    } else {
        m_icon = QIcon();
        m_color = QColor(Qt::white);
    }

    update();
    emit iconChanged(v);
}

QColor IconView::backgroundColor() const
{
    return m_backgroundColor;
}

void IconView::setBackgroundColor(const QColor& color)
{
    if (m_backgroundColor == color) {
        return;
    }

    m_backgroundColor = color;

    update();
    emit backgroundColorChanged(color);
}

void IconView::paint(QPainter* p)
{
    if (m_backgroundColor.isValid()) {
        p->fillRect(0, 0, width(), height(), m_backgroundColor);
    }

    if (m_icon.isNull()) {
        p->fillRect(0, 0, width(), height(), m_color);
        return;
    }

    m_icon.paint(p, QRect(0, 0, width(), height()), Qt::AlignCenter);
}
