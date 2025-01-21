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
#ifndef AU_APPSHELL_APPSHELLCONFIGURATION_H
#define AU_APPSHELL_APPSHELLCONFIGURATION_H

#include "async/asyncable.h"

#include "modularity/ioc.h"
#include "iglobalconfiguration.h"
#include "io/ifilesystem.h"
// #include "multiinstances/imultiinstancesprovider.h"
#include "ui/iuiconfiguration.h"
#include "projectscene/iprojectsceneconfiguration.h"
// #include "project/iprojectconfiguration.h"
// #include "playback/iplaybackconfiguration.h"
// #include "languages/ilanguagesconfiguration.h"

#include "iappshellconfiguration.h"

namespace au::appshell {
class AppShellConfiguration : public IAppShellConfiguration, public muse::async::Asyncable
{
    muse::Inject<muse::IGlobalConfiguration> globalConfiguration;
    muse::Inject<muse::io::IFileSystem> fileSystem;
    muse::Inject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;
    // INJECT(mi::IMultiInstancesProvider, multiInstancesProvider)
    // INJECT(ui::IUiConfiguration, uiConfiguration)
    // INJECT(project::IProjectConfiguration, projectConfiguration)
    // INJECT(notation::INotationConfiguration, notationConfiguration)
    // INJECT(playback::IPlaybackConfiguration, playbackConfiguration)
    // INJECT(languages::ILanguagesConfiguration, languagesConfiguration)

public:
    void init();

    bool hasCompletedFirstLaunchSetup() const override;
    void setHasCompletedFirstLaunchSetup(bool has) override;

    StartupModeType startupModeType() const override;
    void setStartupModeType(StartupModeType type) override;

    muse::io::path_t startupScorePath() const override;
    void setStartupScorePath(const muse::io::path_t& scorePath) override;

    muse::io::path_t userDataPath() const override;

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
    muse::async::Notification isNotationNavigatorVisibleChanged() const override;

    bool needShowSplashScreen() const override;
    void setNeedShowSplashScreen(bool show) override;

    void startEditSettings() override;
    void applySettings() override;
    void rollbackSettings() override;

    void revertToFactorySettings(bool keepDefaultSettings = false, bool notifyAboutChanges = true) const override;

    muse::io::paths_t sessionProjectsPaths() const override;
    muse::Ret setSessionProjectsPaths(const muse::io::paths_t& paths) override;

    muse::ValCh<bool> isEffectsPanelVisible() const override;
    void setIsEffectsPanelVisible(bool visible) override;

private:
    std::string utmParameters(const std::string& utmMedium) const;

    std::string currentLanguageCode() const;

    muse::io::path_t sessionDataPath() const;
    muse::io::path_t sessionFilePath() const;

    muse::RetVal<muse::ByteArray> readSessionState() const;
    muse::Ret writeSessionState(const QByteArray& data);

    muse::io::paths_t parseSessionProjectsPaths(const QByteArray& json) const;
};
}

#endif // AU_APPSHELL_APPSHELLCONFIGURATION_H
