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
#include "application.h"

#include <QApplication>
#include <QProcess>

using namespace mu;

void Application::setRunMode(const RunMode& mode)
{
    m_runMode = mode;
}

IApplication::RunMode Application::runMode() const
{
    return m_runMode;
}

bool Application::noGui() const
{
    switch (m_runMode) {
    case RunMode::GuiApp: return false;
    case RunMode::ConsoleApp: return true;
    case RunMode::AudioPluginRegistration: return true;
    }
    return false;
}

QWindow* Application::focusWindow() const
{
    return qApp->focusWindow();
}

bool Application::notify(QObject* object, QEvent* event)
{
    return qApp->notify(object, event);
}

void Application::restart()
{
    QString program = qApp->arguments()[0];

    // NOTE: remove the first argument - the program name
    QStringList arguments = qApp->arguments().mid(1);

    QCoreApplication::exit();

    QProcess::startDetached(program, arguments);
}
