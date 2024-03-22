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

#ifndef MU_DOCK_DOCKSEPARATOR_H
#define MU_DOCK_DOCKSEPARATOR_H

#include <QQuickItem>

#include "thirdparty/KDDockWidgets/src/private/multisplitter/Separator_p.h"
#include "thirdparty/KDDockWidgets/src/private/multisplitter/Widget_quick.h"

namespace mu::dock {
class DockSeparator : public QQuickItem, public Layouting::Separator, public Layouting::Widget_quick
{
    Q_OBJECT

    Q_PROPERTY(bool isVertical READ isVertical NOTIFY isVerticalChanged)
    Q_PROPERTY(bool isSeparatorVisible READ isSeparatorVisible NOTIFY isSeparatorVisibleChanged)
    Q_PROPERTY(bool showResizeCursor READ showResizeCursor NOTIFY showResizeCursorChanged)

public:
    explicit DockSeparator(Layouting::Widget* parent = nullptr);

    bool isVertical() const;
    bool isSeparatorVisible() const;
    bool showResizeCursor() const;

    Q_INVOKABLE void onMousePressed();
    Q_INVOKABLE void onMouseMoved(QPointF localPos);
    Q_INVOKABLE void onMouseReleased();
    Q_INVOKABLE void onMouseDoubleClicked();

signals:
    void isVerticalChanged();
    void isSeparatorVisibleChanged();
    void showResizeCursorChanged();

private:
    void initAvailability();

    Widget* createRubberBand(Widget* parent) override;
    Widget* asWidget() override;

    bool m_isSeparatorVisible = false;
};
}

#endif // MU_DOCK_DOCKSEPARATOR_H
