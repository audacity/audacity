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
#ifndef MU_APPSHELL_STARTUPSCENARIO_H
#define MU_APPSHELL_STARTUPSCENARIO_H

#include "istartupscenario.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iinteractive.h"
#include "actions/iactionsdispatcher.h"
#include "multiinstances/imultiinstancesprovider.h"
#include "iappshellconfiguration.h"
#include "isessionsmanager.h"
#include "project/iprojectautosaver.h"

namespace mu::appshell {
class StartupScenario : public IStartupScenario, public async::Asyncable
{
    INJECT(IInteractive, interactive)
    INJECT(actions::IActionsDispatcher, dispatcher)
    INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(ISessionsManager, sessionsManager)
    INJECT(project::IProjectAutoSaver, projectAutoSaver)

public:

    void setStartupType(const std::optional<std::string>& type) override;

    bool isStartWithNewFileAsSecondaryInstance() const override;

    const project::ProjectFile& startupScoreFile() const override;
    void setStartupScoreFile(const std::optional<project::ProjectFile>& file) override;

    void run() override;
    bool startupCompleted() const override;

private:
    void onStartupPageOpened(StartupModeType modeType);

    StartupModeType resolveStartupModeType() const;
    Uri startupPageUri(StartupModeType modeType) const;

    void openScore(const project::ProjectFile& file);

    void restoreLastSession();
    void removeProjectsUnsavedChanges(const io::paths_t& projectsPaths);

    std::string m_startupTypeStr;
    project::ProjectFile m_startupScoreFile;
    bool m_startupCompleted = false;
};
}

#endif // MU_APPSHELL_STARTUPSCENARIO_H
