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
#ifndef MU_PROJECT_RECENTSCORESMODEL_H
#define MU_PROJECT_RECENTSCORESMODEL_H

#include "abstractscoresmodel.h"

#include "async/asyncable.h"

#include "iprojectconfiguration.h"
#include "irecentfilescontroller.h"
#include "io/ifilesystem.h"

namespace mu::project {
class RecentScoresModel : public AbstractScoresModel, public muse::async::Asyncable
{
    Q_OBJECT

    INJECT(IProjectConfiguration, configuration)
    INJECT(IRecentFilesController, recentFilesController)
    INJECT(muse::io::IFileSystem, fileSystem)

public:
    RecentScoresModel(QObject* parent = nullptr);

    void load() override;

    QList<int> nonScoreItemIndices() const override;

private:
    void updateRecentScores();
    void setRecentScores(const std::vector<QVariantMap>& items);
};
}

#endif // MU_PROJECT_RECENTSCORESMODEL_H
