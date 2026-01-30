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

#include "sessionsmanager.h"

#include "appshell/appshelltypes.h"

using namespace au::appshell;
using namespace muse;

void SessionsManager::init()
{
    update();

    globalContext()->currentProjectChanged().onNotify(this, [this]() {
        update();

        if (auto project = globalContext()->currentProject()) {
            project->pathChanged().onNotify(this, [this]() {
                update();
            });
        }
    });
}

void SessionsManager::deinit()
{
#ifdef MUSE_MODULE_MULTIWINDOWS
    bool isServer = multiwindowsProvider()->isMainInstance();
    if (!isServer) {
        return;
    }
#endif

    if (configuration()->startupModeType() != StartupModeType::ContinueLastSession) {
        reset();
    }
}

bool SessionsManager::hasProjectsForRestore()
{
    return !configuration()->sessionProjectsPaths().empty();
}

void SessionsManager::restore()
{
    io::paths_t projects = configuration()->sessionProjectsPaths();
    if (projects.empty()) {
        return;
    }

    for (const io::path_t& path : projects) {
        dispatcher()->dispatch("file-open", actions::ActionData::make_arg1<QUrl>(path.toQUrl()));
    }
}

void SessionsManager::reset()
{
    removeProjectsUnsavedChanges(configuration()->sessionProjectsPaths());
    configuration()->setSessionProjectsPaths({});
}

void SessionsManager::update()
{
    io::path_t newProjectPath;

    if (auto project = globalContext()->currentProject()) {
        newProjectPath = project->path();
    }

    if (newProjectPath == m_lastOpenedProjectPath) {
        return;
    }

    if (!m_lastOpenedProjectPath.empty()) {
        removeProjectFromSession(m_lastOpenedProjectPath);
    }

    if (!newProjectPath.empty()) {
        addProjectToSession(newProjectPath);
    }

    m_lastOpenedProjectPath = newProjectPath;
}

void SessionsManager::removeProjectFromSession(const io::path_t& projectPath)
{
    io::paths_t projects = configuration()->sessionProjectsPaths();
    if (projects.empty()) {
        return;
    }

    projects.erase(std::remove(projects.begin(), projects.end(), projectPath), projects.end());
    configuration()->setSessionProjectsPaths(projects);
}

void SessionsManager::addProjectToSession(const io::path_t& projectPath)
{
    io::paths_t projects = configuration()->sessionProjectsPaths();

    if (std::find(projects.begin(), projects.end(), projectPath) == projects.end()) {
        projects.push_back(projectPath);
    }

    configuration()->setSessionProjectsPaths(projects);
}

void SessionsManager::removeProjectsUnsavedChanges(const muse::io::paths_t& projectsPaths)
{
    for (const muse::io::path_t& path : projectsPaths) {
        removeUnsavedChanges(path);
    }
}

void SessionsManager::removeUnsavedChanges(const muse::io::path_t& projectPath)
{
    const Ret ret = au3ProjectCreator()->removeUnsavedData(projectPath);
    if (!ret) {
        LOGE() << "Failed remove autosave data for project: " << projectPath << ", err: " << ret.toString();
    }
}
