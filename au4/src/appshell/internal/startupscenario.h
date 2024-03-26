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
#ifndef AU_APPSHELL_STARTUPSCENARIO_H
#define AU_APPSHELL_STARTUPSCENARIO_H

#include "istartupscenario.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iinteractive.h"
#include "actions/iactionsdispatcher.h"
#include "iappshellconfiguration.h"
#include "isessionsmanager.h"

//! TODO AU4
// #include "multiinstances/imultiinstancesprovider.h"
// #include "project/iprojectautosaver.h"

namespace au::appshell {
class StartupScenario : public au::appshell::IStartupScenario, public mu::async::Asyncable
{
    INJECT(mu::IInteractive, interactive)
    INJECT(mu::actions::IActionsDispatcher, dispatcher)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(ISessionsManager, sessionsManager)

//! TODO AU4
    // INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    // INJECT(project::IProjectAutoSaver, projectAutoSaver)
public:

    void setStartupType(const std::optional<std::string>& type) override;

    bool isStartWithNewFileAsSecondaryInstance() const override;

    const au::project::ProjectFile& startupScoreFile() const override;
    void setStartupScoreFile(const std::optional<au::project::ProjectFile>& file) override;

    void run() override;
    bool startupCompleted() const override;

private:
    void onStartupPageOpened(StartupModeType modeType);

    StartupModeType resolveStartupModeType() const;
    mu::Uri startupPageUri(StartupModeType modeType) const;

    void openScore(const au::project::ProjectFile& file);

    void restoreLastSession();
    void removeProjectsUnsavedChanges(const mu::io::paths_t& projectsPaths);

    std::string m_startupTypeStr;
    au::project::ProjectFile m_startupScoreFile;
    bool m_startupCompleted = false;
};
}

#endif // AU_APPSHELL_STARTUPSCENARIO_H
