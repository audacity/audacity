/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2023 MuseScore BVBA and others
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

#include "scorespagemodel.h"

#include <QString>

#include "actions/actiontypes.h"

#include "types/projecttypes.h"

using namespace mu::project;
using namespace muse::actions;

ScoresPageModel::ScoresPageModel(QObject* parent)
    : QObject(parent)
{
}

void ScoresPageModel::createNewScore()
{
    dispatcher()->dispatch("file-new");
}

void ScoresPageModel::openOther()
{
    dispatcher()->dispatch("file-open");
}

void ScoresPageModel::openScore(const QString& scorePath, const QString& displayNameOverride)
{
    dispatcher()->dispatch("file-open", ActionData::make_arg2<QUrl, QString>(QUrl::fromLocalFile(scorePath), displayNameOverride));
}

void ScoresPageModel::openScoreManager()
{
    interactive()->openUrl(museScoreComService()->scoreManagerUrl());
}

int ScoresPageModel::tabIndex() const
{
    return configuration()->homeScoresPageTabIndex();
}

void ScoresPageModel::setTabIndex(int index)
{
    if (index == tabIndex()) {
        return;
    }

    configuration()->setHomeScoresPageTabIndex(index);
    emit tabIndexChanged();
}

ScoresPageModel::ViewType ScoresPageModel::viewType() const
{
    return static_cast<ViewType>(configuration()->homeScoresPageViewType());
}

void ScoresPageModel::setViewType(ViewType type)
{
    if (viewType() == type) {
        return;
    }

    configuration()->setHomeScoresPageViewType(IProjectConfiguration::HomeScoresPageViewType(type));
    emit viewTypeChanged();
}
