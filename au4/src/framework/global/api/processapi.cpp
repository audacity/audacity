/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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
#include "processapi.h"

using namespace mu::api;

static std::vector<std::string> toArgs(const QStringList& list)
{
    std::vector<std::string> args;
    for (const QString& a : list) {
        args.push_back(a.toStdString());
    }
    return args;
}

ProcessApi::ProcessApi(IApiEngine* e)
    : ApiObject(e)
{
}

int ProcessApi::execute(const QString& program, const QStringList& list)
{
    return process()->execute(program.toStdString(), toArgs(list));
}

bool ProcessApi::startDetached(const QString& program, const QStringList& list)
{
    return process()->startDetached(program.toStdString(), toArgs(list));
}
