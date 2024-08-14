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

#ifndef MU_APP_APP_H
#define MU_APP_APP_H

#include <QList>

#include "modularity/imodulesetup.h"
#include "modularity/ioc.h"
#include "appshell/istartupscenario.h"
#include "audioplugins/iregisteraudiopluginsscenario.h"
#include "appshell/iappshellconfiguration.h"

#include "commandlineparser.h"

namespace au::app {
class App
{
    muse::Inject<muse::IApplication> muapplication;
    muse::Inject<appshell::IStartupScenario> startupScenario;
    muse::Inject<muse::audioplugins::IRegisterAudioPluginsScenario> registerAudioPluginsScenario;
    muse::Inject<appshell::IAppShellConfiguration> appshellConfiguration;

public:
    App();

    void addModule(muse::modularity::IModuleSetup* module);

    int run(int argc, char** argv);

private:
    void applyCommandLineOptions(const CommandLineParser::Options& options);
    int processAudioPluginRegistration(const CommandLineParser::AudioPluginRegistration& task);

    QList<muse::modularity::IModuleSetup*> m_modules;
};
}

#endif // MU_APP_APP_H
