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

#include "startupscenario.h"

#include "framework/global/log.h"
#include "framework/global/types/uri.h"

#include "appshell/appshelltypes.h"

using namespace au::appshell;
using namespace muse::actions;
using namespace au::project;

static const muse::UriQuery FIRST_LAUNCH_SETUP_URI("audacity://firstLaunchSetup?floating=true");
static const muse::Uri ALPHA_WELCOME_POPUP("audacity://alphaWelcomePopup");
static const muse::UriQuery WELCOME_DIALOG_URI("audacity://welcomedialog");
static const muse::Uri HOME_URI("audacity://home");
static const muse::Uri PROJECT_URI("audacity://project");

static StartupModeType modeTypeFromString(const std::string& str)
{
    if ("start-empty" == str) {
        return StartupModeType::StartEmpty;
    }

    if ("continue-last" == str) {
        return StartupModeType::ContinueLastSession;
    }

    if ("start-with-new" == str) {
        return StartupModeType::StartWithNewProject;
    }

    if ("start-with-file" == str) {
        return StartupModeType::StartWithProject;
    }

    return StartupModeType::StartEmpty;
}

void StartupScenario::setStartupType(const std::optional<std::string>& type)
{
    m_startupTypeStr = type ? type.value() : "";
}

bool StartupScenario::isStartWithNewFileAsSecondaryInstance() const
{
    if (m_startupProjectFile.isValid()) {
        return false;
    }

    if (!m_startupTypeStr.empty()) {
        return modeTypeFromString(m_startupTypeStr) == StartupModeType::StartWithNewProject;
    }

    return false;
}

const ProjectFile& StartupScenario::startupProjectFile() const
{
    return m_startupProjectFile;
}

void StartupScenario::setStartupProjectFile(const std::optional<ProjectFile>& file)
{
    m_startupProjectFile = file ? file.value() : ProjectFile();
}

const muse::io::paths_t& StartupScenario::startupMediaFiles() const
{
    return m_startupMediaFiles;
}

void StartupScenario::setStartupMediaFiles(const muse::io::paths_t& files)
{
    m_startupMediaFiles = files;
}

muse::async::Promise<muse::Ret> StartupScenario::runOnSplashScreen()
{
    return muse::async::make_promise<muse::Ret>([this](auto resolve, auto) {
        const muse::Ret ret = muse::make_ret(muse::Ret::Code::Ok);
        return resolve(ret);
    });
}

void StartupScenario::runAfterSplashScreen()
{
    TRACEFUNC;

    if (m_startupCompleted) {
        return;
    }

    StartupModeType modeType = resolveStartupModeType();
    if (multiwindowsProvider()->isFirstWindow() && sessionsManager()->hasProjectsForRestore()) {
        modeType = StartupModeType::Recovery;
    }
    if (multiwindowsProvider()->isFirstWindow() && !configuration()->hasCompletedFirstLaunchSetup()) {
        modeType = StartupModeType::FirstLaunch;
    }

    muse::Uri startupUri = startupPageUri(modeType);

    muse::async::Channel<muse::Uri> opened = interactive()->opened();
    opened.onReceive(this, [this, opened, modeType](const muse::Uri&) {
        if (m_startupCompleted) {
            return;
        }

        m_startupCompleted = true;

        muse::async::Channel<muse::Uri> mut = opened;
        mut.disconnect(this);

        effectsProviderInitializer()->callAfterSplashScreen();

        onStartupPageOpened(modeType);
    });

    interactive()->open(startupUri);
}

bool StartupScenario::startupCompleted() const
{
    return m_startupCompleted;
}

StartupModeType StartupScenario::resolveStartupModeType() const
{
    if (m_startupProjectFile.isValid()) {
        return StartupModeType::StartWithProject;
    }

    if (!m_startupMediaFiles.empty()) {
        return StartupModeType::StartEmpty;
    }

    if (!m_startupTypeStr.empty()) {
        return modeTypeFromString(m_startupTypeStr);
    }

    return configuration()->startupModeType();
}

void StartupScenario::onStartupPageOpened(StartupModeType modeType)
{
    TRACEFUNC;

    showStartupDialogsIfNeed(modeType);

    if (!m_startupMediaFiles.empty()) {
        QStringList files;
        for (const auto& file : m_startupMediaFiles) {
            files << file.toQString();
        }

        dispatcher()->dispatch("project-import-startup-media", ActionData::make_arg1<QStringList>(files));
        return;
    }

    switch (modeType) {
    case StartupModeType::StartEmpty:
        break;
    case StartupModeType::StartWithNewProject:
        dispatcher()->dispatch("file-new");
        break;
    case StartupModeType::ContinueLastSession:
        dispatcher()->dispatch("continue-last-session");
        break;
    case StartupModeType::Recovery:
        restoreLastSession();
        break;
    case StartupModeType::StartWithProject: {
        ProjectFile file = m_startupProjectFile.isValid()
                           ? m_startupProjectFile
                           : ProjectFile(configuration()->startupProjectPath());
        openProject(file);
    } break;
    case StartupModeType::FirstLaunch: {
        dispatcher()->dispatch("file-new");
    } break;
    }
}

void StartupScenario::showStartupDialogsIfNeed(StartupModeType)
{
    const auto showWelcomePage = [this]() {
        const std::string welcomeDialogLastShownVersion(configuration()->welcomeDialogLastShownVersion());
        const std::string currentAudacityVersion(configuration()->audacityVersion());

        if (welcomeDialogLastShownVersion < currentAudacityVersion) {
            configuration()->setWelcomeDialogShowOnStartup(true); // override user preference
            configuration()->setWelcomeDialogLastShownIndex(-1); // reset
        }

        if (!configuration()->welcomeDialogShowOnStartup()) {
            return;
        }

        muse::UriQuery query(WELCOME_DIALOG_URI);
        query.set("modal", false);
        query.set("floating", true);
        interactive()->open(query);

        configuration()->setWelcomeDialogLastShownVersion(configuration()->audacityVersion());
    };

    if (!configuration()->hasCompletedFirstLaunchSetup()) {
        interactive()->open(FIRST_LAUNCH_SETUP_URI).then(this, [this, showWelcomePage](const muse::Val&, auto resolve) {
            showWelcomePage();
            return resolve();
        });
    } else {
        showWelcomePage();
    }
}

muse::Uri StartupScenario::startupPageUri(StartupModeType modeType) const
{
    switch (modeType) {
    case StartupModeType::StartEmpty:
    case StartupModeType::StartWithNewProject:
    case StartupModeType::Recovery:
        return HOME_URI;
    case StartupModeType::StartWithProject:
    case StartupModeType::ContinueLastSession:
    case StartupModeType::FirstLaunch:
        return PROJECT_URI;
    }

    return HOME_URI;
}

void StartupScenario::openProject(const ProjectFile& file)
{
    dispatcher()->dispatch("file-open", ActionData::make_arg2<QUrl, QString>(file.url, file.displayNameOverride));
}

void StartupScenario::restoreLastSession()
{
    muse::IInteractive::Result result = interactive()->questionSync(muse::trc("appshell", "The previous session quit unexpectedly."),
                                                                    muse::trc("appshell", "Do you want to restore the session?"),
                                                                    { muse::IInteractive::Button::No, muse::IInteractive::Button::Yes });

    if (result.button() == static_cast<int>(muse::IInteractive::Button::Yes)) {
        sessionsManager()->restore();
    } else {
        sessionsManager()->reset();
    }
}
