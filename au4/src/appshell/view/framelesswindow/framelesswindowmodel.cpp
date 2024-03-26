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

#include "framelesswindowmodel.h"

#include <QWindow>

#ifdef Q_OS_WIN
#include "internal/platform/win/winframelesswindowcontroller.h"
#else
#include "internal/framelesswindowcontroller.h"
#endif

using namespace au::appshell;

FramelessWindowModel::FramelessWindowModel(QObject* parent)
    : QObject(parent)
{
#ifdef Q_OS_WIN
    m_controller = new WinFramelessWindowController();
#else
    m_controller = new FramelessWindowController();
#endif
}

FramelessWindowModel::~FramelessWindowModel()
{
    delete m_controller;
}

void FramelessWindowModel::init()
{
    m_controller->init();
}

QRect FramelessWindowModel::titleBarMoveArea() const
{
    return m_controller->windowTitleBarMoveArea();
}

void FramelessWindowModel::setTitleBarMoveArea(const QRect& area)
{
    if (titleBarMoveArea() == area) {
        return;
    }

    m_controller->setWindowTitleBarMoveArea(area);
    emit titleBarMoveAreaChanged(area);
}
