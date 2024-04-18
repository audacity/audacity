/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore Limited
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
#ifndef MU_PROJECT_PROJECTAUTOSAVER_H
#define MU_PROJECT_PROJECTAUTOSAVER_H

#include <QTimer>

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "io/ifilesystem.h"
#include "iprojectconfiguration.h"

#include "../iprojectautosaver.h"

namespace mu::project {
class ProjectAutoSaver : public IProjectAutoSaver, public muse::async::Asyncable
{
    INJECT(context::IGlobalContext, globalContext)
    INJECT(muse::io::IFileSystem, fileSystem)
    INJECT(IProjectConfiguration, configuration)

public:
    ProjectAutoSaver() = default;

    void init();

    bool projectHasUnsavedChanges(const muse::io::path_t& projectPath) const override;
    void removeProjectUnsavedChanges(const muse::io::path_t& projectPath) override;

    bool isAutosaveOfNewlyCreatedProject(const muse::io::path_t& projectPath) const override;

    muse::io::path_t projectOriginalPath(const muse::io::path_t& projectAutoSavePath) const override;
    muse::io::path_t projectAutoSavePath(const muse::io::path_t& projectPath) const override;

private:
    INotationProjectPtr currentProject() const;

    void update();

    void onTrySave();

    muse::io::path_t projectPath(INotationProjectPtr project) const;

    QTimer m_timer;
    muse::io::path_t m_lastProjectPathNeedingAutosave;
};
}

#endif // MU_PROJECT_PROJECTAUTOSAVER_H
