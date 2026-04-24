/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
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

#ifndef AU_PROJECT_IOPENSAVEPROJECTSCENARIO_H
#define AU_PROJECT_IOPENSAVEPROJECTSCENARIO_H

#include "modularity/imoduleinterface.h"
#include "global/types/retval.h"
#include "project/iaudacityproject.h"

namespace au::project {
class IOpenSaveProjectScenario : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IOpenSaveProjectScenario)

public:
    virtual muse::RetVal<SaveLocation> askSaveLocation(IAudacityProjectPtr project, SaveMode mode,
                                                       SaveLocationType preselectedType = SaveLocationType::Undefined) const = 0;

    virtual muse::RetVal<muse::io::path_t> askLocalPath(IAudacityProjectPtr project, SaveMode mode) const = 0;
    virtual muse::RetVal<CloudProjectInfo> askCloudLocation(IAudacityProjectPtr project, SaveMode mode) const = 0;
    virtual muse::RetVal<CloudProjectInfo> askPublishLocation(IAudacityProjectPtr project) const = 0;
    virtual muse::RetVal<CloudAudioInfo> askShareAudioLocation(IAudacityProjectPtr project) const = 0;

    virtual muse::RetVal<muse::io::path_t> resolveLegacyProjectFormat(const muse::io::path_t& path) const = 0;

    virtual bool warnBeforeSavingToExistingPubliclyVisibleCloudProject() const = 0;

    static constexpr int RET_CODE_CONFLICT_RESPONSE_SAVE_AS = 1235;
    static constexpr int RET_CODE_CONFLICT_RESPONSE_PUBLISH_AS_NEW_SCORE = 1236;
    static constexpr int RET_CODE_CONFLICT_RESPONSE_REPLACE = 1237;

    static constexpr int RET_CODE_OPEN_LOCAL = 1238;
    static constexpr int RET_CODE_SAVE_LOCALLY = 1239;
    static constexpr int RET_CODE_SAVE_TO_CLOUD = 1240;
    static constexpr int RET_CODE_OPEN_CLOUD_FORCE = 1241;
    static constexpr int RET_CODE_CLOSE_AND_OPEN_CLOUD_FORCE = 1242;
    static constexpr int RET_CODE_SAVE_LOCALLY_AND_REMOVE_CACHE = 1243;
    static constexpr int RET_CODE_SAVE_TO_CLOUD_FORCE = 1244;
    static constexpr int RET_CODE_OPEN_ON_AUDIOCOM = 1245;
    static constexpr int RET_CODE_LOAD_LATEST_SYNCED = 1246;

    virtual muse::Ret showCloudOpenError(const muse::Ret& ret, const muse::io::path_t& localPath) const = 0;
    virtual muse::Ret showCloudSaveError(const muse::Ret& ret) const = 0;
    virtual muse::Ret showCloudAudioOpenError(const muse::Ret& ret) const = 0;
    virtual muse::Ret showAudioCloudShareError(const muse::Ret& ret) const = 0;
};
}

#endif // AU_PROJECT_IOPENSAVEPROJECTSCENARIO_H
