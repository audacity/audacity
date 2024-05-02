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
#include "projectscenemodule.h"

#include <QtQml>

#include "view/toolbars/projecttoolbarmodel.h"
#include "view/trackspanel/trackslistmodel.h"

#include "view/clipsview/waveview.h"
#include "view/clipsview/clipsmodel.h"
#include "view/clipsview/trackclipsitem.h"
#include "view/clipsview/clipitem.h"

#include "types/tracktypes.h"

using namespace au::projectscene;
using namespace muse::modularity;

static void projectscene_init_qrc()
{
    Q_INIT_RESOURCE(projectscene);
}

std::string ProjectSceneModule::moduleName() const
{
    return "projectscene";
}

void ProjectSceneModule::registerExports()
{
}

void ProjectSceneModule::resolveImports()
{
}

void ProjectSceneModule::registerUiTypes()
{
    // toolbars
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");

    // tracks panel
    qmlRegisterType<TracksListModel>("Audacity.ProjectScene", 1, 0, "TracksListModel");
    qmlRegisterUncreatableType<TrackTypes>("Audacity.ProjectScene", 1, 0, "TrackType",
                                           "Not creatable as it is an enum type");

    // clips view
    qmlRegisterType<WaveView>("Audacity.ProjectScene", 1, 0, "WaveView");
    qmlRegisterType<ClipsModel>("Audacity.ProjectScene", 1, 0, "ClipsModel");
    qmlRegisterUncreatableType<WaveSource>("Audacity.ProjectScene", 1, 0, "WaveSource", "Not creatable from QML");
    qmlRegisterUncreatableType<TrackClipsItem>("Audacity.ProjectScene", 1, 0, "TrackClipsItem", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipItem>("Audacity.ProjectScene", 1, 0, "ClipItem", "Not creatable from QML");
}

void ProjectSceneModule::registerResources()
{
    projectscene_init_qrc();
}
