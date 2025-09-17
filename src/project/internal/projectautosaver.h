/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
 *
 * Copyright (C) 2024 Audacity Limited
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
#ifndef AU_PROJECT_PROJECTAUTOSAVER_H
#define AU_PROJECT_PROJECTAUTOSAVER_H

#include <QTimer>

#include "global/async/asyncable.h"
#include "global/modularity/ioc.h"
#include "global/io/ifilesystem.h"
#include "context/iglobalcontext.h"

#include "iprojectconfiguration.h"
#include "../iprojectautosaver.h"
#include "au3wrap/iau3project.h"
// maybe use the #include "appshell/internal/isessionsmanager.h"

namespace au::project {
class ProjectAutoSaver : public IProjectAutoSaver, public muse::async::Asyncable
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<muse::io::IFileSystem> fileSystem;
    muse::Inject<IProjectConfiguration> configuration;
    muse::Inject<au::au3::IAu3ProjectCreator> au3ProjectCreator;
    // maybe use the muse::Inject<au::appshell::ISessionsManager> sessionsManager;

public:
    ProjectAutoSaver() = default;

    void init();

private:
    IAudacityProjectPtr currentProject() const;

    void update();

    void onTrySave();

    QTimer m_timer;
    muse::io::path_t m_lastProjectPathNeedingAutosave;
};
}

#endif // AU_PROJECT_PROJECTAUTOSAVER_H
