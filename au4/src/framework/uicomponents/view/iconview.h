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

#ifndef MU_UICOMPONENTS_ICONVIEW_H
#define MU_UICOMPONENTS_ICONVIEW_H

#include <QIcon>
#include <QColor>

#include "quickpaintedview.h"

namespace mu::uicomponents {
class IconView : public QuickPaintedView
{
    Q_OBJECT

    Q_PROPERTY(QVariant icon READ icon WRITE setIcon NOTIFY iconChanged)
    Q_PROPERTY(QColor backgroundColor READ backgroundColor WRITE setBackgroundColor NOTIFY backgroundColorChanged)

public:
    IconView(QQuickItem* parent = nullptr);

    QVariant icon() const;
    QColor backgroundColor() const;

public slots:
    void setIcon(QVariant val);
    void setBackgroundColor(const QColor& color);

signals:
    void iconChanged(const QVariant& icon);
    void backgroundColorChanged(const QColor& color);

private:
    void paint(QPainter*) override;

    QColor m_color;
    QColor m_backgroundColor;
    QIcon m_icon;
};
}

#endif // MU_UICOMPONENTS_ICONVIEW_H
