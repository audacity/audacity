/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#include "pixmapprojectthumbnailview.h"

#include <QPainterPath>

using namespace au::project;

PixmapProjectThumbnailView::PixmapProjectThumbnailView(QQuickItem* parent)
    : muse::uicomponents::QuickPaintedView(parent)
{
}

QPixmap PixmapProjectThumbnailView::thumbnail() const
{
    return m_thumbnail;
}

void PixmapProjectThumbnailView::setThumbnail(QPixmap pixmap)
{
    m_thumbnail = std::move(pixmap);
    emit thumbnailChanged();
    update();
}

QColor PixmapProjectThumbnailView::borderColor() const
{
    return m_borderColor;
}

void PixmapProjectThumbnailView::setBorderColor(const QColor& color)
{
    if (m_borderColor == color) {
        return;
    }
    m_borderColor = color;
    emit borderColorChanged();
    update();
}

qreal PixmapProjectThumbnailView::radius() const
{
    return m_radius;
}

void PixmapProjectThumbnailView::setRadius(qreal r)
{
    if (qFuzzyCompare(m_radius, r)) {
        return;
    }
    m_radius = r;
    emit radiusChanged();
    update();
}

void PixmapProjectThumbnailView::paint(QPainter* painter)
{
    painter->setRenderHint(QPainter::Antialiasing, true);

    const QRectF rect(0, 0, width(), height());

    if (m_radius > 0.0) {
        QPainterPath clipPath;
        clipPath.addRoundedRect(rect, m_radius, m_radius);
        painter->setClipPath(clipPath);
    }

    painter->drawPixmap(0, 0, width(), height(), m_thumbnail);

    if (m_borderColor.isValid() && m_borderColor.alpha() > 0) {
        painter->setClipping(false);
        painter->setPen(QPen(m_borderColor, 1));
        painter->setBrush(Qt::NoBrush);
        const QRectF borderRect = rect.adjusted(0.5, 0.5, -0.5, -0.5);
        if (m_radius > 0.0) {
            painter->drawRoundedRect(borderRect, m_radius, m_radius);
        } else {
            painter->drawRect(borderRect);
        }
    }
}
