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
#include "dockstatusbarview.h"

using namespace mu::dock;

DockStatusBarView::DockStatusBarView(QQuickItem* parent)
    : DockBase(DockType::StatusBar, parent)
{
    constexpr int STATUS_BAR_HEIGHT = 28;

    setMinimumHeight(STATUS_BAR_HEIGHT);
    setMaximumHeight(STATUS_BAR_HEIGHT);
    setHeight(STATUS_BAR_HEIGHT);

    setFloatable(false);
}
