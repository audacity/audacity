/*
 * SPDX-License-Identifier: GPL-3.0-only
 * Audacity-CLA-applies
 *
 * Audacity
 * Music Composition & Notation
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
#include "projectmodule.h"

#include "modularity/ioc.h"

#include "internal/projectconfiguration.h"
#include "internal/projectuiactions.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "context/iglobalcontext.h"
#include "internal/audacityproject.h"

#include "view/projectspagemodel.h"
#include "view/recentprojectsmodel.h"
#include "view/cloudprojectsmodel.h"
#include "view/cloudprojectstatuswatcher.h"
#include "view/projectthumbnailloader.h"
#include "view/pixmapprojectthumbnailview.h"
#include "view/newprojectmodel.h"

using namespace au::project;
using namespace muse::modularity;

static void project_init_qrc()
{
    Q_INIT_RESOURCE(project);
}

std::string ProjectModule::moduleName() const
{
    return "project";
}

void ProjectModule::registerExports()
{
    m_actionsController = std::make_shared<ProjectActionsController>();

    ioc()->registerExport<IProjectConfiguration>(moduleName(), new ProjectConfiguration());
}

void ProjectModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(std::make_shared<ProjectUiActions>(m_actionsController));
    }
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://project/new"), "Audacity/Project/NewProjectDialog.qml");
    }
}

void ProjectModule::registerResources()
{
    project_init_qrc();
}

void ProjectModule::registerUiTypes()
{
    qmlRegisterType<ProjectsPageModel>("Audacity.Project", 1, 0, "ProjectsPageModel");
    qmlRegisterUncreatableType<AbstractProjectsModel>("Audacity.Project", 1, 0, "AbstractProjectsModel",
                                                      "Not creatable as it is an abstract type");
    qmlRegisterType<RecentProjectsModel>("Audacity.Project", 1, 0, "RecentProjectsModel");
    qmlRegisterType<CloudProjectsModel>("Audacity.Project", 1, 0, "CloudProjectsModel");
    qmlRegisterType<NewProjectModel>("Audacity.Project", 1, 0, "NewProjectModel");
    qmlRegisterType<ProjectThumbnailLoader>("Audacity.Project", 1, 0, "ProjectThumbnailLoader");
    qmlRegisterType<PixmapProjectThumbnailView>("Audacity.Project", 1, 0, "PixmapProjectThumbnailView");
}

void ProjectModule::onInit(const muse::IApplication::RunMode&)
{
    m_actionsController->init();
}

void ProjectModule::onDeinit()
{
}
