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
#ifndef AU_APPSHELL_ISTARTUPSCENARIO_H
#define AU_APPSHELL_ISTARTUPSCENARIO_H

#include <optional>

#include "modularity/imoduleinterface.h"

#include "project/types/projecttypes.h"

namespace au::appshell {
class IStartupScenario : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IStartupScenario)

public:
    virtual ~IStartupScenario() = default;

    virtual void setStartupType(const std::optional<std::string>& type) = 0;

    virtual bool isStartWithNewFileAsSecondaryInstance() const = 0;

    virtual const project::ProjectFile& startupScoreFile() const = 0;
    virtual void setStartupScoreFile(const std::optional<project::ProjectFile>& file) = 0;

    virtual void runOnSplashScreen() = 0;
    virtual void runAfterSplashScreen() = 0;
    virtual bool startupCompleted() const = 0;
};
}

#endif // AU_APPSHELL_ISTARTUPSCENARIO_H
