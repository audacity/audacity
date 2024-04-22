/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-Studio-CLA-applies
 *
 * MuseScore Studio
 * Music Composition & Notation
 *
 * Copyright (C) 2022 MuseScore Limited
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

#ifndef MU_PROJECT_IOPENSAVEPROJECTSCENARIO_H
#define MU_PROJECT_IOPENSAVEPROJECTSCENARIO_H

#include "modularity/imoduleinterface.h"
#include "inotationproject.h"

#include "types/retval.h"

namespace mu::project {
class IOpenSaveProjectScenario : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IOpenSaveProjectScenario)

public:
    virtual muse::RetVal<SaveLocation> askSaveLocation(INotationProjectPtr project, SaveMode mode,
                                                       SaveLocationType preselectedType = SaveLocationType::Undefined) const = 0;

    virtual muse::RetVal<muse::io::path_t> askLocalPath(INotationProjectPtr project, SaveMode mode) const = 0;
    virtual muse::RetVal<CloudProjectInfo> askCloudLocation(INotationProjectPtr project, SaveMode mode) const = 0;
    virtual muse::RetVal<CloudProjectInfo> askPublishLocation(INotationProjectPtr project) const = 0;
    virtual muse::RetVal<CloudAudioInfo> askShareAudioLocation(INotationProjectPtr project) const = 0;

    virtual bool warnBeforeSavingToExistingPubliclyVisibleCloudProject() const = 0;

    static constexpr int RET_CODE_CONFLICT_RESPONSE_SAVE_AS = 1235;
    static constexpr int RET_CODE_CONFLICT_RESPONSE_PUBLISH_AS_NEW_SCORE = 1236;
    static constexpr int RET_CODE_CONFLICT_RESPONSE_REPLACE = 1237;

    virtual void showCloudOpenError(const muse::Ret& ret) const = 0;
    virtual muse::Ret showCloudSaveError(const muse::Ret& ret, const CloudProjectInfo& info, bool publishMode,
                                         bool alreadyAttempted) const = 0;
    virtual muse::Ret showAudioCloudShareError(const muse::Ret& ret) const = 0;
};
}

#endif // MU_PROJECT_IOPENSAVEPROJECTSCENARIO_H
