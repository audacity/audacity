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
#ifndef AU_APPSHELL_IAPPSHELLCONFIGURATION_H
#define AU_APPSHELL_IAPPSHELLCONFIGURATION_H

#include "modularity/imoduleinterface.h"
#include "types/ret.h"
#include "types/retval.h"

#include "io/path.h"
#include "appshelltypes.h"
#include "async/notification.h"

namespace au::appshell {
class IAppShellConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IAppshellConfiguration)

public:
    virtual ~IAppShellConfiguration() = default;

    virtual bool hasCompletedFirstLaunchSetup() const = 0;
    virtual void setHasCompletedFirstLaunchSetup(bool has) = 0;

    virtual StartupModeType startupModeType() const = 0;
    virtual void setStartupModeType(StartupModeType type) = 0;

    virtual muse::io::path_t startupScorePath() const = 0;
    virtual void setStartupScorePath(const muse::io::path_t& scorePath) = 0;

    virtual muse::io::path_t userDataPath() const = 0;

    virtual std::string handbookUrl() const = 0;
    virtual std::string askForHelpUrl() const = 0;
    virtual std::string museScoreUrl() const = 0;
    virtual std::string museScoreForumUrl() const = 0;
    virtual std::string museScoreContributionUrl() const = 0;
    virtual std::string musicXMLLicenseUrl() const = 0;
    virtual std::string musicXMLLicenseDeedUrl() const = 0;

    virtual std::string museScoreVersion() const = 0;
    virtual std::string museScoreRevision() const = 0;

    virtual bool isNotationNavigatorVisible() const = 0;
    virtual void setIsNotationNavigatorVisible(bool visible) const = 0;
    virtual muse::async::Notification isNotationNavigatorVisibleChanged() const = 0;

    virtual bool needShowSplashScreen() const = 0;
    virtual void setNeedShowSplashScreen(bool show) = 0;

    virtual void startEditSettings() = 0;
    virtual void applySettings() = 0;
    virtual void rollbackSettings() = 0;

    virtual void revertToFactorySettings(bool keepDefaultSettings = false, bool notifyAboutChanges = true) const = 0;

    virtual muse::io::paths_t sessionProjectsPaths() const = 0;
    virtual muse::Ret setSessionProjectsPaths(const muse::io::paths_t& paths) = 0;

    virtual muse::ValCh<bool> isEffectsPanelVisible() const = 0;
    virtual void setIsEffectsPanelVisible(bool visible) = 0;
};
}

#endif // AU_APPSHELL_IAPPSHELLCONFIGURATION_H
