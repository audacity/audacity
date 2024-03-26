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

#include "macosscrollinghook.h"

#include <QWheelEvent>
#include <QApplication>

using namespace au::appshell;

void MacOSScrollingHook::init()
{
    qApp->installEventFilter(this);
}

bool MacOSScrollingHook::eventFilter(QObject*, QEvent* event)
{
    if (event->type() != QEvent::Wheel) {
        return false;
    }

    QWheelEvent* wheelEvent = dynamic_cast<QWheelEvent*>(event);
    if (wheelEvent->phase() == Qt::ScrollEnd) {
        return true;
    }

    return false;
}
