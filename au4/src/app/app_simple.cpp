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

#include "app.h"

#include <QApplication>
#include <QQmlApplicationEngine>
#include <QQuickWindow>

#include "log.h"

using namespace mu::app;

App::App()
{
}

// void App::addModule(modularity::IModuleSetup* module)
// {
//     m_modules.push_back(module);
// }

int App::run(int argc, char** argv)
{
    LOGI() << "Hello world";
    return 0;
}
