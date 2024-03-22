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

#include "mainwindow.h"

#include <QWindow>

#include "view/mainwindowbridge.h"
#include "async/notification.h"

#include "log.h"

using namespace mu::ui;
using namespace mu::modularity;

void MainWindow::init(MainWindowBridge* bridge)
{
    IF_ASSERT_FAILED(!m_bridge) {
        LOGW() << "MainWindowBridge is already set. Refusing to set it again.";
        return;
    }

    m_bridge = bridge;

    m_bridge->isFullScreenChanged().onNotify(this, [this]() {
        m_isFullScreenChanged.notify();
    });
}

void MainWindow::deinit()
{
    m_bridge = nullptr;
}

QWindow* MainWindow::qWindow() const
{
    return m_bridge ? m_bridge->qWindow() : nullptr;
}

void MainWindow::requestShowOnBack()
{
    if (!m_bridge) {
        return;
    }

    m_bridge->showOnBack();
}

void MainWindow::requestShowOnFront()
{
    if (!m_bridge) {
        return;
    }

    m_bridge->showOnFront();
}

bool MainWindow::isFullScreen() const
{
    return m_bridge ? m_bridge->isFullScreen() : false;
}

mu::async::Notification MainWindow::isFullScreenChanged() const
{
    return m_isFullScreenChanged;
}

void MainWindow::toggleFullScreen()
{
    if (!m_bridge) {
        return;
    }

    m_bridge->toggleFullScreen();
}

QScreen* MainWindow::screen() const
{
    return m_bridge ? m_bridge->screen() : nullptr;
}
