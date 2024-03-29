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

#include "modularity/ioc.h"
#include "ui/iuiactionsregister.h"

#include "internal/projectsceneactioncontroller.h"
#include "internal/playbackcontroller.h"

#include "view/projectsceneuiactions.h"

#include "view/toolbars/playbacktoolbarmodel.h"
#include "view/toolbars/playbacktoolbarcustomisemodel.h"
#include "view/toolbars/playbacktoolbarcustomiseitem.h"
#include "view/toolbars/projecttoolbarmodel.h"

#include "view/trackspanel/trackslistmodel.h"

#include "view/clipsview/waveview.h"
#include "view/clipsview/clipsmodel.h"
#include "view/clipsview/trackclipsitem.h"
#include "view/clipsview/clipitem.h"

using namespace au::projectscene;
using namespace mu::modularity;

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
    m_actionController = std::make_shared<ProjectSceneActionController>();
    m_projectSceneUiActions = std::make_shared<ProjectSceneUiActions>();

    ioc()->registerExport<IPlaybackController>(moduleName(), new PlaybackController());
}

void ProjectSceneModule::resolveImports()
{
    auto ar = ioc()->resolve<mu::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_projectSceneUiActions);
    }
}

void ProjectSceneModule::registerUiTypes()
{
    // toolbars
    qmlRegisterType<PlaybackToolBarModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarModel");
    qmlRegisterType<PlaybackToolBarCustomiseModel>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseModel");
    qmlRegisterUncreatableType<PlaybackToolBarCustomiseItem>("Audacity.ProjectScene", 1, 0, "PlaybackToolBarCustomiseItem",
                                                             "Cannot create");
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");

    // tracks panel
    qmlRegisterType<TracksListModel>("Audacity.ProjectScene", 1, 0, "TracksListModel");

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

void ProjectSceneModule::onInit(const mu::IApplication::RunMode&)
{
    m_actionController->init();
}

void ProjectSceneModule::onDeinit()
{
}
