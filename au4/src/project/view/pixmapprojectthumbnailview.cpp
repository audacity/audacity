/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
#include "pixmapscorethumbnailview.h"

using namespace mu::project;

PixmapScoreThumbnailView::PixmapScoreThumbnailView(QQuickItem* parent)
    : muse::uicomponents::QuickPaintedView(parent)
{
}

QPixmap PixmapScoreThumbnailView::thumbnail() const
{
    return m_thumbnail;
}

void PixmapScoreThumbnailView::setThumbnail(QPixmap pixmap)
{
    m_thumbnail = std::move(pixmap);
    emit thumbnailChanged();
    update();
}

void PixmapScoreThumbnailView::paint(QPainter* painter)
{
    painter->drawPixmap(0, 0, width(), height(), m_thumbnail);
}
