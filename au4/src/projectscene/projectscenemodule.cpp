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
#include "view/toolbars/playtoolbarmodel.h"

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
    qmlRegisterType<PlayToolBarModel>("Audacity.ProjectScene", 1, 0, "PlayToolBarModel");
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
