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

#include "abstractitemmodel.h"

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "irecentfilescontroller.h"
#include "io/ifilesystem.h"
#include "au3cloud/iau3audiocomservice.h"

namespace au::project {
class RecentProjectsModel : public AbstractItemModel, public muse::async::Asyncable, public muse::Contextable
{
    Q_OBJECT

    muse::GlobalInject<muse::io::IFileSystem> fileSystem;
    muse::GlobalInject<IRecentFilesController> recentFilesController;

    muse::ContextInject<au3cloud::IAu3AudioComService> audioComService { this };

public:
    RecentProjectsModel(QObject* parent = nullptr);

    void load() override;

    QList<int> nonProjectItemIndices() const override;

private:
    void updateRecentProjects();
    void setRecentProjects(const std::vector<QVariantMap>& items);
    void clearContextMenuModels();
};
}

#endif // AU_PROJECT_RECENTPROJECTSMODEL_H
