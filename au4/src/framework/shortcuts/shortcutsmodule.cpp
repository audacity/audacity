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
#include "shortcutsmodule.h"

#include <QtQml>

#include "log.h"

#include "modularity/ioc.h"

#if defined(Q_OS_MACOS)
#include "internal/platform/macos/macosshortcutsinstancemodel.h"
#else
#include "internal/shortcutsinstancemodel.h"
#endif

#include "internal/shortcutsregister.h"
#include "internal/shortcutscontroller.h"
#include "internal/midiremote.h"
#include "internal/shortcutsconfiguration.h"

#include "view/shortcutsmodel.h"
#include "view/editshortcutmodel.h"
#include "view/mididevicemappingmodel.h"
#include "view/editmidimappingmodel.h"

#include "global/api/iapiregister.h"
#include "api/shortcutsapi.h"

#include "ui/iuiengine.h"

#include "diagnostics/idiagnosticspathsregister.h"

using namespace mu::shortcuts;
using namespace mu::modularity;
using namespace mu::ui;

static void shortcuts_init_qrc()
{
    Q_INIT_RESOURCE(shortcuts);
}

std::string ShortcutsModule::moduleName() const
{
    return "shortcuts";
}

void ShortcutsModule::registerExports()
{
    m_shortcutsController = std::make_shared<ShortcutsController>();
    m_shortcutsRegister = std::make_shared<ShortcutsRegister>();
    m_configuration = std::make_shared<ShortcutsConfiguration>();
    m_midiRemote = std::make_shared<MidiRemote>();

    ioc()->registerExport<IShortcutsRegister>(moduleName(), m_shortcutsRegister);
    ioc()->registerExport<IShortcutsController>(moduleName(), m_shortcutsController);
    ioc()->registerExport<IMidiRemote>(moduleName(), m_midiRemote);
    ioc()->registerExport<IShortcutsConfiguration>(moduleName(), m_configuration);
}

void ShortcutsModule::registerApi()
{
    using namespace mu::api;

    auto api = ioc()->resolve<IApiRegister>(moduleName());
    if (api) {
        api->regApiCreator(moduleName(), "api.shortcuts", new ApiCreator<ShortcutsApi>());
    }
}

void ShortcutsModule::registerResources()
{
    shortcuts_init_qrc();
}

void ShortcutsModule::registerUiTypes()
{
#if defined(Q_OS_MACOS)
    qmlRegisterType<MacOSShortcutsInstanceModel>("MuseScore.Shortcuts", 1, 0, "ShortcutsInstanceModel");
#else
    qmlRegisterType<ShortcutsInstanceModel>("MuseScore.Shortcuts", 1, 0, "ShortcutsInstanceModel");
#endif

    qmlRegisterType<ShortcutsModel>("MuseScore.Shortcuts", 1, 0, "ShortcutsModel");
    qmlRegisterType<EditShortcutModel>("MuseScore.Shortcuts", 1, 0, "EditShortcutModel");
    qmlRegisterType<MidiDeviceMappingModel>("MuseScore.Shortcuts", 1, 0, "MidiDeviceMappingModel");
    qmlRegisterType<EditMidiMappingModel>("MuseScore.Shortcuts", 1, 0, "EditMidiMappingModel");

    ioc()->resolve<IUiEngine>(moduleName())->addSourceImportPath(shortcuts_QML_IMPORT);
}

void ShortcutsModule::onInit(const IApplication::RunMode& mode)
{
    if (mode != IApplication::RunMode::GuiApp) {
        return;
    }

    m_configuration->init();
    m_shortcutsController->init();
    m_shortcutsRegister->init();
    m_midiRemote->init();

    auto pr = ioc()->resolve<diagnostics::IDiagnosticsPathsRegister>(moduleName());
    if (pr) {
        pr->reg("shortcutsUserAppDataPath", m_configuration->shortcutsUserAppDataPath());
        pr->reg("shortcutsAppDataPath", m_configuration->shortcutsAppDataPath());
        pr->reg("midiMappingUserAppDataPath", m_configuration->midiMappingUserAppDataPath());
    }
}
