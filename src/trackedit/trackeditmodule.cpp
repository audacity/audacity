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
#include "trackeditmodule.h"

#include "modularity/ioc.h"

#include "internal/trackedituiactions.h"
#include "internal/trackeditactionscontroller.h"
#include "internal/trackeditinteraction.h"

#include "internal/au3/au3trackeditproject.h"
#include "internal/au3/au3interaction.h"
#include "internal/au3/au3selectioncontroller.h"
#include "internal/au3/au3projecthistory.h"
#include "internal/au3/au3trackeditclipboard.h"

#include "ui/iuiactionsregister.h"

using namespace au::trackedit;
using namespace muse;
using namespace muse::modularity;
using namespace muse::ui;
using namespace muse::actions;

std::string TrackeditModule::moduleName() const
{
    return "trackedit";
}

void TrackeditModule::registerExports()
{
    m_trackeditController = std::make_shared<TrackeditActionsController>();
    m_trackeditUiActions = std::make_shared<TrackeditUiActions>(m_trackeditController);
    m_selectionController = std::make_shared<Au3SelectionController>();

    ioc()->registerExport<ITrackeditActionsController>(moduleName(), m_trackeditController);
    ioc()->registerExport<ITrackeditProjectCreator>(moduleName(), new Au3TrackeditProjectCreator());
    ioc()->registerExport<ITrackeditInteraction>(moduleName(), new TrackeditInteraction(std::make_unique<Au3Interaction>()));
    ioc()->registerExport<ISelectionController>(moduleName(), m_selectionController);
    ioc()->registerExport<IProjectHistory>(moduleName(), new Au3ProjectHistory());
    ioc()->registerExport<ITrackeditClipboard>(moduleName(), new Au3TrackeditClipboard());
}

void TrackeditModule::resolveImports()
{
    auto ar = ioc()->resolve<muse::ui::IUiActionsRegister>(moduleName());
    if (ar) {
        ar->reg(m_trackeditUiActions);
    }
}

void TrackeditModule::onInit(const muse::IApplication::RunMode&)
{
    m_trackeditController->init();
    m_trackeditUiActions->init();
    m_selectionController->init();

    TimeSignatureRestorer::reg();
}

void TrackeditModule::onDeinit()
{
}
