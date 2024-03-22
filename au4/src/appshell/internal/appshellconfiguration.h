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
#ifndef MU_APPSHELL_APPSHELLCONFIGURATION_H
#define MU_APPSHELL_APPSHELLCONFIGURATION_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iglobalconfiguration.h"
#include "io/ifilesystem.h"
#include "multiinstances/imultiinstancesprovider.h"
#include "ui/iuiconfiguration.h"
#include "project/iprojectconfiguration.h"
#include "notation/inotationconfiguration.h"
#include "playback/iplaybackconfiguration.h"
#include "languages/ilanguagesconfiguration.h"

#include "iappshellconfiguration.h"

namespace mu::appshell {
class AppShellConfiguration : public IAppShellConfiguration, public async::Asyncable
{
    INJECT(IGlobalConfiguration, globalConfiguration)
    INJECT(io::IFileSystem, fileSystem)
    INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    INJECT(ui::IUiConfiguration, uiConfiguration)
    INJECT(project::IProjectConfiguration, projectConfiguration)
    INJECT(notation::INotationConfiguration, notationConfiguration)
    INJECT(playback::IPlaybackConfiguration, playbackConfiguration)
    INJECT(languages::ILanguagesConfiguration, languagesConfiguration)

public:
    void init();

    bool hasCompletedFirstLaunchSetup() const override;
    void setHasCompletedFirstLaunchSetup(bool has) override;

    StartupModeType startupModeType() const override;
    void setStartupModeType(StartupModeType type) override;

    io::path_t startupScorePath() const override;
    void setStartupScorePath(const io::path_t& scorePath) override;

    io::path_t userDataPath() const override;

    std::string handbookUrl() const override;
    std::string askForHelpUrl() const override;
    std::string museScoreUrl() const override;
    std::string museScoreForumUrl() const override;
    std::string museScoreContributionUrl() const override;
    std::string musicXMLLicenseUrl() const override;
    std::string musicXMLLicenseDeedUrl() const override;

    std::string museScoreVersion() const override;
    std::string museScoreRevision() const override;

    bool isNotationNavigatorVisible() const override;
    void setIsNotationNavigatorVisible(bool visible) const override;
    async::Notification isNotationNavigatorVisibleChanged() const override;

    bool needShowSplashScreen() const override;
    void setNeedShowSplashScreen(bool show) override;

    void startEditSettings() override;
    void applySettings() override;
    void rollbackSettings() override;

    void revertToFactorySettings(bool keepDefaultSettings = false, bool notifyAboutChanges = true) const override;

    io::paths_t sessionProjectsPaths() const override;
    Ret setSessionProjectsPaths(const io::paths_t& paths) override;

private:
    std::string utmParameters(const std::string& utmMedium) const;

    std::string currentLanguageCode() const;

    io::path_t sessionDataPath() const;
    io::path_t sessionFilePath() const;

    RetVal<mu::ByteArray> readSessionState() const;
    Ret writeSessionState(const QByteArray& data);

    io::paths_t parseSessionProjectsPaths(const QByteArray& json) const;
};
}

#endif // MU_APPSHELL_APPSHELLCONFIGURATION_H
