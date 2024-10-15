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

void PixmapProjectThumbnailView::paint(QPainter* painter)
{
    painter->drawPixmap(0, 0, width(), height(), m_thumbnail);
}
