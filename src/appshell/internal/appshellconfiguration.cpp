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
#include "appshellconfiguration.h"

#include <QJsonArray>
#include <QJsonDocument>

#include "framework/global/settings.h"
#include "framework/global/log.h"

#include "appshell/appshelltypes.h"

// #include "multiwindows/resourcelockguard.h"

using namespace muse;
using namespace au::appshell;

static const std::string module_name("appshell");

static const Settings::Key HAS_COMPLETED_FIRST_LAUNCH_SETUP(module_name, "application/hasCompletedFirstLaunchSetup");

static const Settings::Key WELCOME_DIALOG_SHOW_ON_STARTUP_KEY(module_name, "application/welcomeDialogShowOnStartup");
static const Settings::Key WELCOME_DIALOG_LAST_SHOWN_VERSION_KEY(module_name, "application/welcomeDialogLastShownVersion");
static const Settings::Key WELCOME_DIALOG_LAST_SHOWN_INDEX(module_name, "application/welcomeDialogLastShownIndex");

static const Settings::Key STARTUP_MODE_TYPE(module_name, "application/startup/modeStart");
static const Settings::Key STARTUP_SCORE_PATH(module_name, "application/startup/startScore");

static const std::string AUDACITY_URL("https://www.audacityteam.org/");
static const std::string AUDACITY_MANUAL_URL("https://manual.audacityteam.org/");
static const std::string AUDACITY_SUPPORT_URL("https://support.audacityteam.org/");
static const std::string AUDACITY_FORUM_URL("https://forum.audacityteam.org/");
static const std::string AUDACITY_CONTRIBUTE_URL("https://support.audacityteam.org/community/contributing");

static const Settings::Key SPLASH_SCREEN_VISIBLE_KEY(module_name, "ui/application/startup/showSplashScreen");

static const muse::io::path_t SESSION_FILE("/session.json");
static const std::string SESSION_RESOURCE_NAME("SESSION");

void AppShellConfiguration::init()
{
    settings()->setDefaultValue(HAS_COMPLETED_FIRST_LAUNCH_SETUP, Val(false));

    settings()->setDefaultValue(WELCOME_DIALOG_SHOW_ON_STARTUP_KEY, Val(true));
    settings()->valueChanged(WELCOME_DIALOG_SHOW_ON_STARTUP_KEY).onReceive(this, [this](const Val&) {
        m_welcomeDialogShowOnStartupChanged.notify();
    });

    settings()->setDefaultValue(WELCOME_DIALOG_LAST_SHOWN_VERSION_KEY, Val("0.0.0"));
    settings()->setDefaultValue(WELCOME_DIALOG_LAST_SHOWN_INDEX, Val(-1));

    settings()->setDefaultValue(STARTUP_MODE_TYPE, Val(StartupModeType::StartEmpty));
    // settings()->setDefaultValue(STARTUP_SCORE_PATH, Val(projectConfiguration()->myFirstProjectPath().toStdString()));

    fileSystem()->makePath(sessionDataPath());
}

bool AppShellConfiguration::hasCompletedFirstLaunchSetup() const
{
    return settings()->value(HAS_COMPLETED_FIRST_LAUNCH_SETUP).toBool();
}

bool AppShellConfiguration::welcomeDialogShowOnStartup() const
{
    return settings()->value(WELCOME_DIALOG_SHOW_ON_STARTUP_KEY).toBool();
}

void AppShellConfiguration::setWelcomeDialogShowOnStartup(bool show)
{
    settings()->setSharedValue(WELCOME_DIALOG_SHOW_ON_STARTUP_KEY, Val(show));
}

async::Notification AppShellConfiguration::welcomeDialogShowOnStartupChanged() const
{
    return m_welcomeDialogShowOnStartupChanged;
}

std::string AppShellConfiguration::welcomeDialogLastShownVersion() const
{
    return settings()->value(WELCOME_DIALOG_LAST_SHOWN_VERSION_KEY).toString();
}

void AppShellConfiguration::setWelcomeDialogLastShownVersion(const std::string& version)
{
    settings()->setSharedValue(WELCOME_DIALOG_LAST_SHOWN_VERSION_KEY, Val(version));
}

int AppShellConfiguration::welcomeDialogLastShownIndex() const
{
    return settings()->value(WELCOME_DIALOG_LAST_SHOWN_INDEX).toInt();
}

void AppShellConfiguration::setWelcomeDialogLastShownIndex(int index)
{
    settings()->setSharedValue(WELCOME_DIALOG_LAST_SHOWN_INDEX, Val(index));
}

void AppShellConfiguration::setHasCompletedFirstLaunchSetup(bool has)
{
    settings()->setSharedValue(HAS_COMPLETED_FIRST_LAUNCH_SETUP, Val(has));
}

StartupModeType AppShellConfiguration::startupModeType() const
{
    return settings()->value(STARTUP_MODE_TYPE).toEnum<StartupModeType>();
}

void AppShellConfiguration::setStartupModeType(StartupModeType type)
{
    settings()->setSharedValue(STARTUP_MODE_TYPE, Val(type));
}

muse::io::path_t AppShellConfiguration::startupProjectPath() const
{
    return settings()->value(STARTUP_SCORE_PATH).toString();
}

void AppShellConfiguration::setStartupProjectPath(const io::path_t& scorePath)
{
    settings()->setSharedValue(STARTUP_SCORE_PATH, Val(scorePath.toStdString()));
}

muse::io::path_t AppShellConfiguration::userDataPath() const
{
    return globalConfiguration()->userDataPath();
}

std::string AppShellConfiguration::handbookUrl() const
{
    return AUDACITY_MANUAL_URL;
}

std::string AppShellConfiguration::askForHelpUrl() const
{
    return AUDACITY_SUPPORT_URL;
}

std::string AppShellConfiguration::appUrl() const
{
    return AUDACITY_URL;
}

std::string AppShellConfiguration::forumUrl() const
{
    return AUDACITY_FORUM_URL;
}

std::string AppShellConfiguration::contributionUrl() const
{
    return AUDACITY_CONTRIBUTE_URL;
}

std::string AppShellConfiguration::audacityVersion() const
{
    return String(application()->version().toString() + u"." + application()->build()).toStdString();
}

std::string AppShellConfiguration::appRevision() const
{
    return std::string();
}

bool AppShellConfiguration::needShowSplashScreen() const
{
    return settings()->value(SPLASH_SCREEN_VISIBLE_KEY).toBool();
}

void AppShellConfiguration::setNeedShowSplashScreen(bool show)
{
    settings()->setSharedValue(SPLASH_SCREEN_VISIBLE_KEY, Val(show));
}

const QString& AppShellConfiguration::preferencesDialogLastOpenedPageId() const
{
    return m_preferencesDialogCurrentPageId;
}

void AppShellConfiguration::setPreferencesDialogLastOpenedPageId(const QString& lastOpenedPageId)
{
    if (m_preferencesDialogCurrentPageId == lastOpenedPageId) {
        return;
    }
    m_preferencesDialogCurrentPageId = lastOpenedPageId;
}

void AppShellConfiguration::startEditSettings()
{
    settings()->beginTransaction();
}

void AppShellConfiguration::applySettings()
{
    settings()->commitTransaction();
    m_settingsApplied.notify();
}

void AppShellConfiguration::rollbackSettings()
{
    settings()->rollbackTransaction();
}

muse::async::Notification AppShellConfiguration::settingsApplied() const
{
    return m_settingsApplied;
}

void AppShellConfiguration::revertToFactorySettings(bool keepDefaultSettings, bool notifyAboutChanges, bool notifyOtherInstances)
{
    m_aboutToRevertToFactorySettings.notify();
    settings()->reset(keepDefaultSettings, notifyAboutChanges, notifyOtherInstances);
}

muse::async::Notification AppShellConfiguration::aboutToRevertToFactorySettings() const
{
    return m_aboutToRevertToFactorySettings;
}

muse::io::paths_t AppShellConfiguration::sessionProjectsPaths() const
{
    RetVal<ByteArray> retVal = readSessionState();
    if (!retVal.ret) {
        LOGE() << retVal.ret.toString();
        return {};
    }

    return parseSessionProjectsPaths(retVal.val.toQByteArrayNoCopy());
}

muse::Ret AppShellConfiguration::setSessionProjectsPaths(const muse::io::paths_t& paths)
{
    QJsonArray jsonArray;
    for (const io::path_t& path : paths) {
        jsonArray << QJsonValue(path.toQString());
    }

    QByteArray data = QJsonDocument(jsonArray).toJson();
    return writeSessionState(data);
}

bool AppShellConfiguration::isEffectsPanelVisible() const
{
    return projectSceneConfiguration()->isEffectsPanelVisible();
}

void AppShellConfiguration::setIsEffectsPanelVisible(bool visible)
{
    projectSceneConfiguration()->setIsEffectsPanelVisible(visible);
}

async::Notification AppShellConfiguration::isEffectsPanelVisibleChanged() const
{
    return projectSceneConfiguration()->isEffectsPanelVisibleChanged();
}

muse::io::path_t AppShellConfiguration::sessionDataPath() const
{
    return globalConfiguration()->userAppDataPath() + "/session";
}

muse::io::path_t AppShellConfiguration::sessionFilePath() const
{
    return sessionDataPath() + SESSION_FILE;
}

muse::RetVal<muse::ByteArray> AppShellConfiguration::readSessionState() const
{
    // mi::ReadResourceLockGuard lock_guard(multiwindowsProvider(), SESSION_RESOURCE_NAME);
    return fileSystem()->readFile(sessionFilePath());
}

muse::Ret AppShellConfiguration::writeSessionState(const QByteArray& data)
{
    // mi::WriteResourceLockGuard lock_guard(multiwindowsProvider(), SESSION_RESOURCE_NAME);
    return fileSystem()->writeFile(sessionFilePath(), ByteArray::fromQByteArrayNoCopy(data));
}

muse::io::paths_t AppShellConfiguration::parseSessionProjectsPaths(const QByteArray& json) const
{
    QJsonParseError err;
    QJsonDocument jsodDoc = QJsonDocument::fromJson(json, &err);
    if (err.error != QJsonParseError::NoError || !jsodDoc.isArray()) {
        LOGE() << "failed parse, err: " << err.errorString();
        return {};
    }

    io::paths_t result;
    const QVariantList pathsList = jsodDoc.array().toVariantList();
    for (const QVariant& pathVal : pathsList) {
        io::path_t path = pathVal.toString().toStdString();
        if (!path.empty()) {
            result.push_back(path);
        }
    }

    return result;
}
