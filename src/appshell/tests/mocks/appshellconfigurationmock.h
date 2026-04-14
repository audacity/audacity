/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "iappshellconfiguration.h"

namespace au::appshell {
class AppShellConfigurationMock : public IAppShellConfiguration
{
public:
    MOCK_METHOD(bool, hasCompletedFirstLaunchSetup, (), (const, override));
    MOCK_METHOD(void, setHasCompletedFirstLaunchSetup, (bool), (override));

    MOCK_METHOD(bool, welcomeDialogShowOnStartup, (), (const, override));
    MOCK_METHOD(void, setWelcomeDialogShowOnStartup, (bool), (override));
    MOCK_METHOD(muse::async::Notification, welcomeDialogShowOnStartupChanged, (), (const, override));

    MOCK_METHOD(std::string, welcomeDialogLastShownVersion, (), (const, override));
    MOCK_METHOD(void, setWelcomeDialogLastShownVersion, (const std::string&), (override));

    MOCK_METHOD(int, welcomeDialogLastShownIndex, (), (const, override));
    MOCK_METHOD(void, setWelcomeDialogLastShownIndex, (int), (override));

    MOCK_METHOD(StartupModeType, startupModeType, (), (const, override));
    MOCK_METHOD(void, setStartupModeType, (StartupModeType), (override));

    MOCK_METHOD(muse::io::path_t, startupProjectPath, (), (const, override));
    MOCK_METHOD(void, setStartupProjectPath, (const muse::io::path_t&), (override));

    MOCK_METHOD(muse::io::path_t, userDataPath, (), (const, override));

    MOCK_METHOD(std::string, handbookUrl, (), (const, override));
    MOCK_METHOD(std::string, askForHelpUrl, (), (const, override));
    MOCK_METHOD(std::string, appUrl, (), (const, override));
    MOCK_METHOD(std::string, forumUrl, (), (const, override));
    MOCK_METHOD(std::string, contributionUrl, (), (const, override));

    MOCK_METHOD(std::string, audacityVersion, (), (const, override));
    MOCK_METHOD(std::string, appRevision, (), (const, override));

    MOCK_METHOD(bool, needShowSplashScreen, (), (const, override));
    MOCK_METHOD(void, setNeedShowSplashScreen, (bool), (override));

    MOCK_METHOD(const QString&, preferencesDialogLastOpenedPageId, (), (const, override));
    MOCK_METHOD(void, setPreferencesDialogLastOpenedPageId, (const QString&), (override));

    MOCK_METHOD(void, startEditSettings, (), (override));
    MOCK_METHOD(void, applySettings, (), (override));
    MOCK_METHOD(void, rollbackSettings, (), (override));
    MOCK_METHOD(muse::async::Notification, settingsApplied, (), (const, override));

    MOCK_METHOD(void, revertToFactorySettings, (bool, bool, bool), (override));
    MOCK_METHOD(muse::async::Notification, aboutToRevertToFactorySettings, (), (const, override));

    MOCK_METHOD(muse::io::paths_t, sessionProjectsPaths, (), (const, override));
    MOCK_METHOD(muse::Ret, setSessionProjectsPaths, (const muse::io::paths_t&), (override));

    MOCK_METHOD(bool, isEffectsPanelVisible, (), (const, override));
    MOCK_METHOD(void, setIsEffectsPanelVisible, (bool), (override));
    MOCK_METHOD(muse::async::Notification, isEffectsPanelVisibleChanged, (), (const, override));
};
}
