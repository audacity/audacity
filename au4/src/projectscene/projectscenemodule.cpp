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

#include "types/projectscenetypes.h"

#include "view/toolbars/projecttoolbarmodel.h"
#include "view/trackspanel/trackslistmodel.h"

#include "view/clipsview/trackslistclipsmodel.h"
#include "view/clipsview/clipslistmodel.h"
#include "view/clipsview/waveview.h"
#include "view/clipsview/timelinecontext.h"

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
    // types
    qmlRegisterUncreatableType<TrackTypes>("Audacity.ProjectScene", 1, 0, "TrackType", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipKey>("Audacity.ProjectScene", 1, 0, "ClipKey", "Not creatable from QML");

    // toolbars
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");

    // tracks panel
    qmlRegisterType<TracksListModel>("Audacity.ProjectScene", 1, 0, "TracksListModel");

    // clips view
    qmlRegisterType<TracksListClipsModel>("Audacity.ProjectScene", 1, 0, "TracksListClipsModel");
    qmlRegisterType<ClipsListModel>("Audacity.ProjectScene", 1, 0, "ClipsListModel");
    qmlRegisterType<TimelineContext>("Audacity.ProjectScene", 1, 0, "TimelineContext");
    qmlRegisterType<WaveView>("Audacity.ProjectScene", 1, 0, "WaveView");
}

void ProjectSceneModule::registerResources()
{
    projectscene_init_qrc();
}
