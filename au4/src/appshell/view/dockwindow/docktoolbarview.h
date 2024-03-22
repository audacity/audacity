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

#ifndef MU_DOCK_DOCKTOOLBARVIEW_H
#define MU_DOCK_DOCKTOOLBARVIEW_H

#include "internal/dockbase.h"

#include <QtGlobal>

namespace mu::dock {
class DockToolBarAlignment
{
    Q_GADGET

public:
    enum Type {
        Left,
        Center,
        Right
    };

    Q_ENUM(Type)
};

class DockToolBarView : public DockBase
{
    Q_OBJECT

    Q_PROPERTY(Qt::Orientation orientation READ orientation WRITE setOrientation NOTIFY orientationChanged)
    Q_PROPERTY(int alignment READ alignment WRITE setAlignment NOTIFY alignmentChanged)

public:
    explicit DockToolBarView(QQuickItem* parent = nullptr);

    Qt::Orientation orientation() const;
    int alignment() const;

    Q_INVOKABLE void setDraggableMouseArea(QQuickItem* mouseArea);

    void init() override;
    void resetToDefault() override;

public slots:
    void setOrientation(Qt::Orientation orientation);
    void setAlignment(int alignment);

signals:
    void orientationChanged(Qt::Orientation orientation);
    void alignmentChanged(int alignment);

private:
    void componentComplete() override;

    bool canChangeOrientation() const;

    class DraggableArea;
    DraggableArea* m_draggableArea = nullptr;

    Qt::Orientation m_orientation = Qt::Horizontal;
    int m_alignment = static_cast<int>(DockToolBarAlignment::Left);
};
}

#endif // MU_DOCK_DOCKTOOLBARVIEW_H
