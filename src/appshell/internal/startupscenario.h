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
#ifndef AU_APPSHELL_STARTUPSCENARIO_H
#define AU_APPSHELL_STARTUPSCENARIO_H

#include "istartupscenario.h"

#include "framework/global/async/asyncable.h"
#include "framework/global/modularity/ioc.h"
#include "framework/interactive/iinteractive.h"
#include "framework/actions/iactionsdispatcher.h"
#include "framework/audioplugins/iregisteraudiopluginsscenario.h"
#include "framework/multiwindows/imultiwindowsprovider.h"

#include "appshell/iappshellconfiguration.h"
#include "appshell/internal/isessionsmanager.h"

namespace au::appshell {
class StartupScenario : public au::appshell::IStartupScenario, public muse::async::Asyncable, public muse::Injectable
{
    muse::GlobalInject<IAppShellConfiguration> configuration;
    muse::GlobalInject<muse::mi::IMultiWindowsProvider> multiwindowsProvider;

    muse::Inject<muse::IInteractive> interactive { this };
    muse::Inject<muse::actions::IActionsDispatcher> dispatcher { this };
    muse::Inject<ISessionsManager> sessionsManager { this };
    muse::Inject<muse::audioplugins::IRegisterAudioPluginsScenario> registerAudioPluginsScenario { this };

public:
    StartupScenario(const muse::modularity::ContextPtr& ctx)
        : muse::Injectable(ctx) {}

    void setStartupType(const std::optional<std::string>& type) override;

    bool isStartWithNewFileAsSecondaryInstance() const override;

    const au::project::ProjectFile& startupScoreFile() const override;
    void setStartupScoreFile(const std::optional<au::project::ProjectFile>& file) override;

    muse::async::Promise<muse::Ret> runOnSplashScreen() override;
    void runAfterSplashScreen() override;
    bool startupCompleted() const override;

private:
    void onStartupPageOpened(StartupModeType modeType);
    void showStartupDialogsIfNeed(StartupModeType modeType);

    StartupModeType resolveStartupModeType() const;
    muse::Uri startupPageUri(StartupModeType modeType) const;

    void openScore(const au::project::ProjectFile& file);

    void restoreLastSession();

    std::string m_startupTypeStr;
    au::project::ProjectFile m_startupScoreFile;
    bool m_startupCompleted = false;
};
}

#endif // AU_APPSHELL_STARTUPSCENARIO_H
