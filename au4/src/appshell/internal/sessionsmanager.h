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
#ifndef MU_APPSHELL_SESSIONSMANAGER_H
#define MU_APPSHELL_SESSIONSMANAGER_H

#include "istartupscenario.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "multiinstances/imultiinstancesprovider.h"
#include "project/iprojectconfiguration.h"
#include "iappshellconfiguration.h"

#include "isessionsmanager.h"

namespace mu::appshell {
class SessionsManager : public ISessionsManager, public async::Asyncable
{
    INJECT(actions::IActionsDispatcher, dispatcher)
    INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    INJECT(context::IGlobalContext, globalContext)
    INJECT(project::IProjectConfiguration, projectConfiguration)
    INJECT(IAppShellConfiguration, configuration)

public:
    void init();
    void deinit();

    bool hasProjectsForRestore() override;

    void restore() override;
    void reset() override;

private:
    void update();

    void removeProjectFromSession(const io::path_t& projectPath);
    void addProjectToSession(const io::path_t& projectPath);

    io::path_t m_lastOpenedProjectPath;
};
}

#endif // MU_APPSHELL_SESSIONSMANAGER_H
