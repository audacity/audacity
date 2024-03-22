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

#include "mainwindowbridge.h"

#include <QWindow>

#include "modularity/ioc.h"
#include "async/notification.h"

#include "log.h"

using namespace mu::ui;
using namespace mu::modularity;

MainWindowBridge::MainWindowBridge(QObject* parent)
    : QObject(parent), m_window(nullptr)
{
}

MainWindowBridge::~MainWindowBridge()
{
    mainWindow()->deinit();
}

QWindow* MainWindowBridge::qWindow() const
{
    return m_window;
}

void MainWindowBridge::setWindow(QWindow* window)
{
    if (m_window != nullptr) {
        LOGW() << "Window for this MainWindowBridge is already set. Refusing to set it again.";
        return;
    }

    m_window = window;
    emit windowChanged();

    init();
}

void MainWindowBridge::init()
{
    mainWindow()->init(this);

    updateFullScreen();
    connect(m_window, &QWindow::windowStateChanged, this, [this]() {
        updateFullScreen();
    });
}

QString MainWindowBridge::filePath() const
{
    return m_window ? m_window->filePath() : "";
}

void MainWindowBridge::setFilePath(const QString& filePath)
{
    if (!m_window) {
        return;
    }

    if (filePath == m_window->filePath()) {
        return;
    }

    m_window->setFilePath(filePath);
    emit filePathChanged();
}

bool MainWindowBridge::fileModified() const
{
    return false;
}

void MainWindowBridge::setFileModified(bool /*modified*/)
{
}

void MainWindowBridge::showOnBack()
{
    m_window->lower();
}

void MainWindowBridge::showOnFront()
{
#ifdef Q_OS_MAC
    // On macOS, this simple way of raising the window works just fine
    // (and the other way causes problems, since calling `show()` resizes the window if it was sized to fill the screen)
    m_window->setWindowStates(m_window->windowStates() & ~Qt::WindowMinimized);
    m_window->raise();
#else
    struct Holder {
        QMetaObject::Connection conn;
    };

    Holder* h = new Holder();
    h->conn = QObject::connect(m_window, &QWindow::activeChanged, [this, h]() {
        if (m_window->isActive()) {
            m_window->raise();
        }

        QObject::disconnect(h->conn);
        delete h;
    });
    m_window->show();
    m_window->requestActivate();
#endif
}

bool MainWindowBridge::isFullScreen() const
{
    return m_isFullScreen;
}

mu::async::Notification MainWindowBridge::isFullScreenChanged() const
{
    return m_isFullScreenChanged;
}

void MainWindowBridge::toggleFullScreen()
{
    if (!m_window) {
        return;
    }

    if (isFullScreen()) {
        m_window->setVisibility(m_windowVisibility);
    } else {
        m_windowVisibility = m_window->visibility();
        m_window->setVisibility(QWindow::FullScreen);
    }
}

void MainWindowBridge::updateFullScreen()
{
    bool isFullScreen = m_window ? m_window->windowStates().testFlag(Qt::WindowFullScreen) : false;
    if (isFullScreen == m_isFullScreen) {
        return;
    }

    m_isFullScreen = isFullScreen;
    m_isFullScreenChanged.notify();
}

QScreen* MainWindowBridge::screen() const
{
    return m_window ? m_window->screen() : nullptr;
}

void MainWindowBridge::showMinimizedWithSavePreviousState()
{
    // On Windows, QWindow::showMinimized() doesn't store the previous state of the window.
    // Thus, it will always be restored to a windowed state once clicked on from the task bar, even
    // if it was maximized before.
    // Using setWindowStates, we can remember the previous state and restore it correctly
    // when the window is shown again.
    m_window->setWindowStates(Qt::WindowMinimized | m_window->windowStates());
}
