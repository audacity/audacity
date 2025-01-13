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
#include "projectmodule.h"

#include "modularity/ioc.h"

#include "internal/projectconfiguration.h"
#include "internal/projectuiactions.h"
#include "internal/thumbnailcreator.h"

#include "ui/iuiactionsregister.h"
#include "ui/iinteractiveuriregister.h"

#include "context/iglobalcontext.h"
#include "internal/audacityproject.h"

#include "view/projectpropertiesmodel.h"
#include "view/projectspagemodel.h"
#include "view/recentprojectsmodel.h"
#include "internal/opensaveprojectscenario.h"
#include "view/cloudprojectsmodel.h"
#include "view/cloudprojectstatuswatcher.h"
#include "view/projectthumbnailloader.h"
#include "view/pixmapprojectthumbnailview.h"
#include "view/newprojectmodel.h"

#ifdef Q_OS_MAC
#include "internal/platform/macos/macosrecentfilescontroller.h"
#elif defined (Q_OS_WIN)
#include "internal/platform/windows/windowsrecentfilescontroller.h"
#else
#include "internal/recentfilescontroller.h"
#endif

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
    m_configuration = std::make_shared<ProjectConfiguration>();
    m_actionsController = std::make_shared<ProjectActionsController>();
    m_uiActions = std::make_shared<ProjectUiActions>(m_actionsController);
    m_thumbnailCreator = std::make_shared<ThumbnailCreator>();

#ifdef Q_OS_MAC
    m_recentFilesController = std::make_shared<MacOSRecentFilesController>();
#elif defined(Q_OS_WIN)
    m_recentFilesController = std::make_shared<WindowsRecentFilesController>();
#else
    m_recentFilesController = std::make_shared<RecentFilesController>();
#endif

    ioc()->registerExport<IProjectConfiguration>(moduleName(), m_configuration);
    ioc()->registerExport<IRecentFilesController>(moduleName(), m_recentFilesController);
    ioc()->registerExport<IOpenSaveProjectScenario>(moduleName(), new OpenSaveProjectScenario());
    ioc()->registerExport<IProjectFilesController>(moduleName(), m_actionsController);
    ioc()->registerExport<IThumbnailCreator>(moduleName(), m_thumbnailCreator);
}

void ProjectModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_uiActions);
    }
    auto ir = ioc()->resolve<muse::ui::IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerQmlUri(muse::Uri("audacity://project/new"), "Audacity/Project/NewProjectDialog.qml");
        ir->registerQmlUri(muse::Uri("audacity://project/asksavelocationtype"), "Audacity/Project/AskSaveLocationTypeDialog.qml");
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
    qmlRegisterType<ProjectPropertiesModel>("Audacity.Project", 1, 0, "ProjectPropertiesModel");

    qmlRegisterUncreatableType<QMLSaveLocationType>("Audacity.Project", 1, 0, "SaveLocationType",
                                                    "Not creatable as it is an enum type");
}

void ProjectModule::onInit(const muse::IApplication::RunMode&)
{
    m_configuration->init();
    m_actionsController->init();
    m_uiActions->init();
    m_recentFilesController->init();
}

void ProjectModule::onDeinit()
{
}
