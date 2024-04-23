/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * A Digital Audio Editor
 *
 * Copyright (C) 2024 Audacity BVBA and others
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
#ifndef AU_PROJECT_RECENTPROJECTSMODEL_H
#define AU_PROJECT_RECENTPROJECTSMODEL_H

#include "abstractprojectsmodel.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iprojectconfiguration.h"
#include "irecentfilescontroller.h"
#include "io/ifilesystem.h"

namespace au::project {
class RecentProjectsModel : public AbstractProjectsModel, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IProjectConfiguration> configuration;
    muse::Inject<IRecentFilesController> recentFilesController;
    muse::Inject<muse::io::IFileSystem> fileSystem;

public:
    RecentProjectsModel(QObject* parent = nullptr);

    void load() override;

    QList<int> nonProjectItemIndices() const override;

private:
    void updateRecentProjects();
    void setRecentProjects(const std::vector<QVariantMap>& items);
};
}

#endif // AU_PROJECT_RECENTPROJECTSMODEL_H
