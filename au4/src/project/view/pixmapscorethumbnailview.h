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
#ifndef MU_PROJECT_PIXMAPSCORETHUMBNAILVIEW_H
#define MU_PROJECT_PIXMAPSCORETHUMBNAILVIEW_H

#include <QPainter>

#include "uicomponents/view/quickpaintedview.h"

namespace mu::project {
class PixmapScoreThumbnailView : public muse::uicomponents::QuickPaintedView
{
    Q_OBJECT

    Q_PROPERTY(QPixmap thumbnail READ thumbnail WRITE setThumbnail NOTIFY thumbnailChanged)

public:
    PixmapScoreThumbnailView(QQuickItem* parent = nullptr);

    QPixmap thumbnail() const;
    void setThumbnail(QPixmap pixmap);

signals:
    void thumbnailChanged();

protected:
    void paint(QPainter* painter) override;

private:
    QPixmap m_thumbnail;
};
}

#endif // MU_PROJECT_PIXMAPSCORETHUMBNAILVIEW_H
