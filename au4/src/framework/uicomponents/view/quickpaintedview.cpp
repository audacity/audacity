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
#include "quickpaintedview.h"

using namespace mu::uicomponents;

QuickPaintedView::QuickPaintedView(QQuickItem* parent)
    : QQuickPaintedItem(parent)
{
    //! NOTE It is necessary that when UI scaling is displayed without a blur
    setAntialiasing(false);
    setSmooth(false);
}

QSGNode* QuickPaintedView::updatePaintNode(QSGNode* old, UpdatePaintNodeData* data)
{
    //! NOTE It is necessary that when UI scaling is displayed without a blur
    setTextureSize(QSize(width(), height()));
    QSGNode* n = QQuickPaintedItem::updatePaintNode(old, data);
    return n;
}
