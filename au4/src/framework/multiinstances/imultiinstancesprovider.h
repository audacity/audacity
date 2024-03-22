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
#ifndef MU_MI_IMULTIINSTANCESPROVIDER_H
#define MU_MI_IMULTIINSTANCESPROVIDER_H

#include <string>
#include <vector>

#include "modularity/imoduleinterface.h"
#include "io/path.h"
#include "mitypes.h"
#include "async/notification.h"
#include "async/channel.h"
#include "types/val.h"

namespace mu::mi {
class IMultiInstancesProvider : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IMultiInstancesProvider)
public:
    virtual ~IMultiInstancesProvider() = default;

    // Project opening
    virtual bool isProjectAlreadyOpened(const io::path_t& projectPath) const = 0;
    virtual void activateWindowWithProject(const io::path_t& projectPath) = 0;
    virtual bool isHasAppInstanceWithoutProject() const = 0;
    virtual void activateWindowWithoutProject() = 0;
    virtual bool openNewAppInstance(const QStringList& args) = 0;

    // Settings
    virtual bool isPreferencesAlreadyOpened() const = 0;
    virtual void activateWindowWithOpenedPreferences() const = 0;
    virtual void settingsBeginTransaction() = 0;
    virtual void settingsCommitTransaction() = 0;
    virtual void settingsRollbackTransaction() = 0;
    virtual void settingsSetValue(const std::string& key, const Val& value) = 0;

    // Resources (files)
    virtual bool lockResource(const std::string& name) = 0;
    virtual bool unlockResource(const std::string& name) = 0;
    virtual void notifyAboutResourceChanged(const std::string& name) = 0;
    virtual async::Channel<std::string> resourceChanged() = 0;

    // Instances info
    virtual const std::string& selfID() const = 0;
    virtual bool isMainInstance() const = 0;
    virtual std::vector<InstanceMeta> instances() const = 0;
    virtual async::Notification instancesChanged() const = 0;

    virtual void notifyAboutInstanceWasQuited() = 0;

    // Quit for all
    virtual void quitForAll() = 0;
    virtual void quitAllAndRestartLast() = 0;
    virtual void quitAllAndRunInstallation(const io::path_t& installerPath) = 0;
};
}

#endif // MU_MI_IMULTIINSTANCESPROVIDER_H
