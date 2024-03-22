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

#include "dockingholderview.h"

using namespace mu::dock;

DockingHolderView::DockingHolderView(QQuickItem* parent)
    : DockBase(DockType::DockingHolder, parent)
{
    setVisible(false);
    setFloatable(false);
    setClosable(false);
}

void DockingHolderView::componentComplete()
{
    constexpr int HOLDER_SIZE = 36;

    switch (location()) {
    case Location::Left:
    case Location::Right:
        setMinimumWidth(HOLDER_SIZE);
        setWidth(HOLDER_SIZE);
        break;
    case Location::Top:
    case Location::Bottom:
        setMinimumHeight(HOLDER_SIZE);
        setHeight(HOLDER_SIZE);
        break;
    case Location::Center:
    case Location::Undefined:
        break;
    }

    DockBase::componentComplete();
}
