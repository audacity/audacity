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
#include "contextmodule.h"

#include "modularity/ioc.h"
#include "internal/globalcontext.h"
#include "internal/uicontextresolver.h"
#include "shortcutcontext.h"

using namespace au::context;
using namespace muse::modularity;
using namespace muse::shortcuts;

std::string ContextModule::moduleName() const
{
    return "context";
}

void ContextModule::registerExports()
{
    m_globalContext = std::make_shared<GlobalContext>();
    m_uicontextResolver = std::make_shared<UiContextResolver>();

    ioc()->registerExport<IGlobalContext>(moduleName(), m_globalContext);
    ioc()->registerExport<IUiContextResolver>(moduleName(), m_uicontextResolver);
    ioc()->registerExport<IShortcutContextPriority>(moduleName(), new ShortcutContextPriority());
}

void ContextModule::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_uicontextResolver->init();
}

void ContextModule::onDeinit()
{
    au::project::IAudacityProjectPtr prj = m_globalContext->currentProject();
    if (prj) {
        prj->close();
    }
    m_globalContext->setCurrentProject(nullptr);
}
