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
#include "processingmodule.h"

#include "modularity/ioc.h"

#include "internal/processinguiactions.h"
#include "internal/processingactionscontroller.h"

#include "ui/iuiactionsregister.h"

using namespace au::processing;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

//! NOTE This is essentially the core of the application;
//! here is the application’s domain model and its main functions for audio processing.

std::string ProcessingModule::moduleName() const
{
    return "processing";
}

void ProcessingModule::registerExports()
{
    m_processingController = std::make_shared<ProcessingActionsController>();
    m_processingUiActions = std::make_shared<ProcessingUiActions>(m_processingController);

    ioc()->registerExport<IProcessingActionsController>(moduleName(), m_processingController);
}

void ProcessingModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(std::make_shared<ProcessingUiActions>(m_processingController));
    }
}

void ProcessingModule::onInit(const muse::IApplication::RunMode&)
{
    m_processingController->init();
    m_processingUiActions->init();
}

void ProcessingModule::onDeinit()
{
}
