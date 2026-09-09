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

static const std::string mname("context");

std::string ContextModule::moduleName() const
{
    return mname;
}

IContextSetup* ContextModule::newContext(const muse::modularity::ContextPtr& ctx) const
{
    return new ContextModuleContext(ctx);
}

void ContextModuleContext::registerExports()
{
    m_globalContext = std::make_shared<GlobalContext>(iocContext());
    m_uicontextResolver = std::make_shared<UiContextResolver>(iocContext());

    ioc()->registerExport<IGlobalContext>(mname, m_globalContext);
    ioc()->registerExport<IUiContextResolver>(mname, m_uicontextResolver);
    ioc()->registerExport<muse::extensions::IExtensionContextResolver>(mname, m_uicontextResolver);
    ioc()->registerExport<IShortcutContextPriority>(mname, new ShortcutContextPriority());
}

void ContextModuleContext::onInit(const muse::IApplication::RunMode& mode)
{
    if (mode != muse::IApplication::RunMode::GuiApp) {
        return;
    }

    m_uicontextResolver->init();
}

void ContextModuleContext::onDeinit()
{
    au::project::IAudacityProjectPtr prj = m_globalContext->currentProject();
    if (prj) {
        prj->close();
    }
    m_globalContext->setCurrentProject(nullptr);
}
