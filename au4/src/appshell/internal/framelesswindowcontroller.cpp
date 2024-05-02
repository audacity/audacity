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

#include "framelesswindowcontroller.h"

using namespace au::appshell;

void FramelessWindowController::init()
{
}

QRect FramelessWindowController::windowTitleBarMoveArea() const
{
    return m_windowTitleBarMoveArea;
}

void FramelessWindowController::setWindowTitleBarMoveArea(const QRect& area)
{
    m_windowTitleBarMoveArea = area;
}

#ifdef MU_QT5_COMPAT
bool FramelessWindowController::nativeEventFilter(const QByteArray& eventType, void* message, long* result)
#else
bool FramelessWindowController::nativeEventFilter(const QByteArray& eventType, void* message, qintptr* result)
#endif
{
    Q_UNUSED(eventType)
    Q_UNUSED(message)
    Q_UNUSED(result)
    return true;
}
