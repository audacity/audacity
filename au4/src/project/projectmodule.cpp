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
#include "projectmodule.h"

#include "modularity/ioc.h"

#include "internal/projectconfiguration.h"
#include "internal/projectuiactions.h"

#include "ui/iuiactionsregister.h"

#include "context/iglobalcontext.h"
#include "internal/audacityproject.h"

using namespace au::project;
using namespace au::project;
using namespace muse::modularity;

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
}

void ProjectModule::onInit(const muse::IApplication::RunMode&)
{
    m_actionsController->init();
}

void ProjectModule::onDeinit()
{
}
