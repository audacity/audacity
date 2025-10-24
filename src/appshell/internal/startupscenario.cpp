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

#include "global/async/async.h"
#include "global/translation.h"
#include "global/log.h"

using namespace au::appshell;
using namespace muse::actions;
using namespace au::project;

static const muse::UriQuery FIRST_LAUNCH_SETUP_URI("musescore://firstLaunchSetup?floating=true");
static const muse::Uri ALPHA_WELCOME_POPUP("audacity://alphaWelcomePopup");
static const muse::Uri HOME_URI("musescore://home");
static const muse::Uri PROJECT_URI("audacity://project");

static StartupModeType modeTypeTromString(const std::string& str)
{
    if ("start-empty" == str) {
        return StartupModeType::StartEmpty;
    }

    if ("continue-last" == str) {
        return StartupModeType::ContinueLastSession;
    }

    if ("start-with-new" == str) {
        return StartupModeType::StartWithNewScore;
    }

    if ("start-with-file" == str) {
        return StartupModeType::StartWithScore;
    }

    return StartupModeType::StartEmpty;
}

void StartupScenario::setStartupType(const std::optional<std::string>& type)
{
    m_startupTypeStr = type ? type.value() : "";
}

bool StartupScenario::isStartWithNewFileAsSecondaryInstance() const
{
    if (m_startupScoreFile.isValid()) {
        return false;
    }

    if (!m_startupTypeStr.empty()) {
        return modeTypeTromString(m_startupTypeStr) == StartupModeType::StartWithNewScore;
    }

    return false;
}

const ProjectFile& StartupScenario::startupScoreFile() const
{
    return m_startupScoreFile;
}

void StartupScenario::setStartupScoreFile(const std::optional<ProjectFile>& file)
{
    m_startupScoreFile = file ? file.value() : ProjectFile();
}

void StartupScenario::runOnSplashScreen()
{
    //! NOTE Registering plugins shows a window (dialog) before the main window is shown.
    //! After closing it, the application may in a state where there are no open windows,
    //! which leads to automatic exit from the application.
    //! (Thanks to the splashscreen, but this is not an obvious detail)
    // qApp->setQuitLockEnabled(false);
    // qApp->setQuitLockEnabled(true);
}

void StartupScenario::runAfterSplashScreen()
{
    TRACEFUNC;

    if (m_startupCompleted) {
        return;
    }

    StartupModeType modeType = resolveStartupModeType();
    //! TODO AU4
    // bool isMainInstance = multiInstancesProvider()->isMainInstance();
    if (/*isMainInstance && */ sessionsManager()->hasProjectsForRestore()) {
        modeType = StartupModeType::Recovery;
    }
    if (!configuration()->hasCompletedFirstLaunchSetup()) {
        modeType = StartupModeType::FirstLaunch;
    }

    muse::Uri startupUri = startupPageUri(modeType);

    muse::async::Channel<muse::Uri> opened = interactive()->opened();
    opened.onReceive(this, [this, opened, modeType](const muse::Uri&) {
        static bool once = false;
        if (once) {
            return;
        }
        once = true;

        muse::io::paths_t newPluginPaths = registerAudioPluginsScenario()->scanForNewPluginPaths();

        if (!newPluginPaths.empty()) {
            auto ret = interactive()->questionSync(muse::trc("appshell", "Scanning audio plugins"),
                                                   muse::trc(
                                                       "appshell",
                                                       "Audacity has found plugins that need to be scanned before use. Would you like to scan them now or skip?"),
                                                   { muse::IInteractive::ButtonData(
                                                         muse::IInteractive::Button::Cancel, muse::trc("appshell", "Skip this time"),
                                                         false),
                                                     muse::IInteractive::ButtonData(
                                                         muse::IInteractive::Button::Apply, muse::trc("appshell", "Scan plugins"), true) });
            if (ret.standardButton() == muse::IInteractive::Button::Apply) {
                muse::Ret ret = registerAudioPluginsScenario()->registerNewPlugins(newPluginPaths);
                if (!ret) {
                    LOGE() << ret.toString();
                }
            }
        }

        onStartupPageOpened(modeType);

        muse::async::Async::call(this, [this, opened]() {
            muse::async::Channel<muse::Uri> mut = opened;
            mut.disconnect(this);
            m_startupCompleted = true;
        });
    });

    interactive()->open(startupUri);
}

bool StartupScenario::startupCompleted() const
{
    return m_startupCompleted;
}

StartupModeType StartupScenario::resolveStartupModeType() const
{
    if (m_startupScoreFile.isValid()) {
        return StartupModeType::StartWithScore;
    }

    if (!m_startupTypeStr.empty()) {
        return modeTypeTromString(m_startupTypeStr);
    }

    return configuration()->startupModeType();
}

void StartupScenario::onStartupPageOpened(StartupModeType modeType)
{
    TRACEFUNC;

#ifndef MUSE_APP_UNSTABLE
    if (modeType != StartupModeType::FirstLaunch) {
        interactive()->openSync(ALPHA_WELCOME_POPUP);
    }
#endif

    switch (modeType) {
    case StartupModeType::StartEmpty:
        break;
    case StartupModeType::StartWithNewScore:
        dispatcher()->dispatch("file-new");
        break;
    case StartupModeType::ContinueLastSession:
        dispatcher()->dispatch("continue-last-session");
        break;
    case StartupModeType::Recovery:
        restoreLastSession();
        break;
    case StartupModeType::StartWithScore: {
        ProjectFile file = m_startupScoreFile.isValid()
                           ? m_startupScoreFile
                           : ProjectFile(configuration()->startupScorePath());
        openScore(file);
    } break;
    case StartupModeType::FirstLaunch: {
        dispatcher()->dispatch("file-new");
        interactive()->openSync(FIRST_LAUNCH_SETUP_URI);
#ifndef MUSE_APP_UNSTABLE
        interactive()->openSync(ALPHA_WELCOME_POPUP);
#endif
    } break;
    }
}

muse::Uri StartupScenario::startupPageUri(StartupModeType modeType) const
{
    switch (modeType) {
    case StartupModeType::StartEmpty:
    case StartupModeType::StartWithNewScore:
    case StartupModeType::Recovery:
        return HOME_URI;
    case StartupModeType::StartWithScore:
    case StartupModeType::ContinueLastSession:
    case StartupModeType::FirstLaunch:
        return PROJECT_URI;
    }

    return HOME_URI;
}

void StartupScenario::openScore(const ProjectFile& file)
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
