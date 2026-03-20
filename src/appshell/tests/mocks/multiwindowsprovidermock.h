/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "multiwindows/imultiwindowsprovider.h"

namespace muse::mi {
class MultiWindowsProviderMock : public IMultiWindowsProvider
{
public:
    MOCK_METHOD(size_t, windowCount, (), (const, override));
    MOCK_METHOD(bool, isFirstWindow, (), (const, override));
    MOCK_METHOD(bool, isProjectAlreadyOpened, (const io::path_t&), (const, override));
    MOCK_METHOD(void, activateWindowWithProject, (const io::path_t&), (override));
    MOCK_METHOD(bool, isHasWindowWithoutProject, (), (const, override));
    MOCK_METHOD(void, activateWindowWithoutProject, (const QStringList&), (override));
    MOCK_METHOD(bool, openNewWindow, (const QStringList&), (override));
    MOCK_METHOD(bool, isPreferencesAlreadyOpened, (), (const, override));
    MOCK_METHOD(void, activateWindowWithOpenedPreferences, (), (const, override));
    MOCK_METHOD(void, settingsBeginTransaction, (), (override));
    MOCK_METHOD(void, settingsCommitTransaction, (), (override));
    MOCK_METHOD(void, settingsRollbackTransaction, (), (override));
    MOCK_METHOD(void, settingsReset, (), (override));
    MOCK_METHOD(void, settingsSetValue, (const std::string&, const Val&), (override));
    MOCK_METHOD(bool, lockResource, (const std::string&), (override));
    MOCK_METHOD(bool, unlockResource, (const std::string&), (override));
    MOCK_METHOD(void, notifyAboutResourceChanged, (const std::string&), (override));
    MOCK_METHOD(async::Channel<std::string>, resourceChanged, (), (override));
    MOCK_METHOD(void, notifyAboutWindowWasQuited, (), (override));
    MOCK_METHOD(void, quitForAll, (), (override));
    MOCK_METHOD(void, quitAllAndRestartLast, (), (override));
    MOCK_METHOD(void, quitAllAndRunInstallation, (const io::path_t&), (override));
};
}
