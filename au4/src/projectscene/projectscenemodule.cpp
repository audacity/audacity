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

#include "ui/iuiactionsregister.h"

#include "internal/projectsceneuiactions.h"
#include "internal/projectsceneactionscontroller.h"
#include "internal/projectsceneconfiguration.h"
#include "internal/projectviewstatecreator.h"

#include "view/common/tracksviewstatemodel.h"

#include "view/toolbars/projecttoolbarmodel.h"
#include "view/trackspanel/trackslistmodel.h"

#include "view/clipsview/trackslistclipsmodel.h"
#include "view/clipsview/clipslistmodel.h"
#include "view/clipsview/waveview.h"
#include "view/timeline/timelinecontext.h"
#include "view/timeline/timelineruler.h"
#include "view/timeline/timelinecontextmenumodel.h"

#include "view/playcursor/playcursorcontroller.h"

using namespace au::projectscene;
using namespace muse::modularity;
using namespace muse::ui;

static void projectscene_init_qrc()
{
    Q_INIT_RESOURCE(projectscene);
}

std::string ProjectSceneModule::moduleName() const
{
    return "projectscene";
}

void ProjectSceneModule::registerResources()
{
    projectscene_init_qrc();
}

void ProjectSceneModule::registerExports()
{
    m_projectSceneActionsController = std::make_shared<ProjectSceneActionsController>();
    m_uiActions = std::make_shared<ProjectSceneUiActions>(m_projectSceneActionsController);
    m_configuration = std::make_shared<ProjectSceneConfiguration>();

    ioc()->registerExport<IProjectSceneConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IProjectViewStateCreator>(moduleName(), new ProjectViewStateCreator());
    ioc()->registerExport<IProjectSceneActionsController>(moduleName(), m_projectSceneActionsController);
}

void ProjectSceneModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(std::make_shared<ProjectSceneUiActions>(m_projectSceneActionsController));
    }
}

void ProjectSceneModule::registerUiTypes()
{
    // types
    qmlRegisterUncreatableType<TrackTypes>("Audacity.ProjectScene", 1, 0, "TrackType", "Not creatable from QML");
    qmlRegisterUncreatableType<ClipKey>("Audacity.ProjectScene", 1, 0, "ClipKey", "Not creatable from QML");

    // common
    qmlRegisterType<TracksViewStateModel>("Audacity.ProjectScene", 1, 0, "TracksViewStateModel");

    // toolbars
    qmlRegisterType<ProjectToolBarModel>("Audacity.ProjectScene", 1, 0, "ProjectToolBarModel");

    // tracks panel
    qmlRegisterType<TracksListModel>("Audacity.ProjectScene", 1, 0, "TracksListModel");

    // clips view
    qmlRegisterType<TracksListClipsModel>("Audacity.ProjectScene", 1, 0, "TracksListClipsModel");
    qmlRegisterType<ClipsListModel>("Audacity.ProjectScene", 1, 0, "ClipsListModel");
    qmlRegisterType<TimelineContext>("Audacity.ProjectScene", 1, 0, "TimelineContext");
    qmlRegisterType<TimelineRuler>("Audacity.ProjectScene", 1, 0, "TimelineRuler");
    qmlRegisterType<TimelineContextMenuModel>("Audacity.ProjectScene", 1, 0, "TimelineContextMenuModel");
    qmlRegisterType<WaveView>("Audacity.ProjectScene", 1, 0, "WaveView");

    // play cursor
    qmlRegisterType<PlayCursorController>("Audacity.ProjectScene", 1, 0, "PlayCursorController");
}

void ProjectSceneModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_configuration->init();
    m_projectSceneActionsController->init();
}
