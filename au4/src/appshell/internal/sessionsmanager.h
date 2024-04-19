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
#ifndef AU_APPSHELL_SESSIONSMANAGER_H
#define AU_APPSHELL_SESSIONSMANAGER_H

#include "istartupscenario.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "actions/iactionsdispatcher.h"
#include "context/iglobalcontext.h"
#include "iappshellconfiguration.h"
#include "multiinstances/imultiinstancesprovider.h"
#include "isessionsmanager.h"

#include "project/iprojectconfiguration.h"

namespace au::appshell {
class SessionsManager : public ISessionsManager, public muse::async::Asyncable
{
    INJECT(muse::actions::IActionsDispatcher, dispatcher)
    INJECT(au::context::IGlobalContext, globalContext)
    INJECT(IAppShellConfiguration, configuration)
    INJECT(muse::mi::IMultiInstancesProvider, multiInstancesProvider)
    INJECT(project::IProjectConfiguration, projectConfiguration)
public:
    void init();
    void deinit();

    bool hasProjectsForRestore() override;

    void restore() override;
    void reset() override;

private:
    void update();

    void removeProjectFromSession(const muse::io::path_t& projectPath);
    void addProjectToSession(const muse::io::path_t& projectPath);

    muse::io::path_t m_lastOpenedProjectPath;
};
}

#endif // AU_APPSHELL_SESSIONSMANAGER_H
