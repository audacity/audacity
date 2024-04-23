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

#ifndef AU_PROJECT_OPENSAVEPROJECTSCENARIO_H
#define AU_PROJECT_OPENSAVEPROJECTSCENARIO_H

#include "iopensaveprojectscenario.h"

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "iprojectfilescontroller.h"
#include "global/iinteractive.h"

#include "cloud/musescorecom/imusescorecomservice.h"
#include "cloud/audiocom/iaudiocomservice.h"
#include "cloud/cloudqmltypes.h"

namespace au::project {
class OpenSaveProjectScenario : public IOpenSaveProjectScenario
{
    muse::Inject<IProjectConfiguration> configuration;
    muse::Inject<IProjectFilesController> projectFilesController;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<muse::cloud::IMuseScoreComService> museScoreComService;
    muse::Inject<muse::cloud::IAudioComService> audioComService;

public:
    OpenSaveProjectScenario() = default;

    muse::RetVal<SaveLocation> askSaveLocation(IAudacityProjectPtr project, SaveMode mode,
                                               SaveLocationType preselectedType = SaveLocationType::Undefined) const override;

    muse::RetVal<muse::io::path_t> askLocalPath(IAudacityProjectPtr project, SaveMode mode) const override;
    muse::RetVal<CloudProjectInfo> askCloudLocation(IAudacityProjectPtr project, SaveMode mode) const override;
    muse::RetVal<CloudProjectInfo> askPublishLocation(IAudacityProjectPtr project) const override;
    muse::RetVal<CloudAudioInfo> askShareAudioLocation(IAudacityProjectPtr project) const override;

    bool warnBeforeSavingToExistingPubliclyVisibleCloudProject() const override;

    void showCloudOpenError(const muse::Ret& ret) const override;
    muse::Ret showCloudSaveError(const muse::Ret& ret, const CloudProjectInfo& info, bool isPublishShare,
                                 bool alreadyAttempted) const override;
    muse::Ret showAudioCloudShareError(const muse::Ret& ret) const override;

private:
    muse::RetVal<SaveLocationType> saveLocationType() const;
    muse::RetVal<SaveLocationType> askSaveLocationType() const;

    /// \param isPublishShare:
    ///     false -> this is part of a "Save to cloud" action
    ///     true -> this is part of a "Publish" action
    muse::RetVal<CloudProjectInfo> doAskCloudLocation(IAudacityProjectPtr project, SaveMode mode, bool isPublishShare) const;

    bool warnBeforePublishing(bool isPublishShare, muse::cloud::Visibility visibility) const;

    muse::Ret warnCloudNotAvailableForUploading(bool isPublishShare) const;
    muse::Ret warnCloudNotAvailableForSharingAudio() const;
};

class QMLSaveLocationType
{
    Q_GADGET

public:
    enum SaveLocationType {
        Undefined = int(project::SaveLocationType::Undefined),
        Local = int(project::SaveLocationType::Local),
        Cloud = int(project::SaveLocationType::Cloud)
    };
    Q_ENUM(SaveLocationType);
};
}

#endif // AU_PROJECT_OPENSAVEPROJECTSCENARIO_H
