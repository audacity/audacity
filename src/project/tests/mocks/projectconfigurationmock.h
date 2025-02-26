/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <gmock/gmock.h>

#include "project/iprojectconfiguration.h"

namespace au::project {
class ProjectConfigurationMock : public project::IProjectConfiguration
{
public:
    MOCK_METHOD(muse::io::path_t, recentFilesJsonPath, (), (const, override));
    MOCK_METHOD(muse::ByteArray, compatRecentFilesData, (), (const, override));

    MOCK_METHOD(muse::io::path_t, userProjectsPath, (), (const, override));
    MOCK_METHOD(void, setUserProjectsPath, (const muse::io::path_t& path), (override));
    MOCK_METHOD(muse::async::Channel<muse::io::path_t>, userProjectsPathChanged, (), (const, override));
    MOCK_METHOD(muse::io::path_t, defaultUserProjectsPath, (), (const, override));

    MOCK_METHOD(muse::io::path_t, lastOpenedProjectsPath, (), (const, override));
    MOCK_METHOD(void, setLastOpenedProjectsPath, (const muse::io::path_t& path), (override));

    MOCK_METHOD(muse::io::path_t, lastSavedProjectsPath, (), (const, override));
    MOCK_METHOD(void, setLastSavedProjectsPath, (const muse::io::path_t& path), (override));

    MOCK_METHOD(muse::io::path_t, defaultSavingFilePath, (IAudacityProjectPtr project, const std::string& filenameAddition,
                                                          const std::string& suffix), (const, override));
    MOCK_METHOD(SaveLocationType, lastUsedSaveLocationType, (), (const, override));
    MOCK_METHOD(void, setLastUsedSaveLocationType, (SaveLocationType type), (override));

    MOCK_METHOD(bool, shouldAskSaveLocationType, (), (const, override));
    MOCK_METHOD(void, setShouldAskSaveLocationType, (bool shouldAsk), (override));

    MOCK_METHOD(muse::io::path_t, temporaryDir, (), (const, override));
    MOCK_METHOD(void, setTemporaryDir, (const muse::io::path_t& path), (override));
    MOCK_METHOD(muse::async::Channel<muse::io::path_t>, temporaryDirChanged, (), (const, override));

    MOCK_METHOD(muse::io::path_t, newProjectTemporaryPath, (), (const, override));

    MOCK_METHOD(int, homeProjectsPageTabIndex, (), (const, override));
    MOCK_METHOD(void, setHomeProjectsPageTabIndex, (int index), (override));

    MOCK_METHOD(HomeProjectsPageViewType, homeProjectsPageViewType, (), (const, override));
    MOCK_METHOD(void, setHomeProjectsPageViewType, (HomeProjectsPageViewType type), (override));

    MOCK_METHOD(bool, isAutoSaveEnabled, (), (const, override));
    MOCK_METHOD(void, setAutoSaveEnabled, (bool enabled), (override));
    MOCK_METHOD(muse::async::Channel<bool>, autoSaveEnabledChanged, (), (const, override));

    MOCK_METHOD(int, autoSaveIntervalMinutes, (), (const, override));
    MOCK_METHOD(void, setAutoSaveInterval, (int minutes), (override));
    MOCK_METHOD(muse::async::Channel<int>, autoSaveIntervalChanged, (), (const, override));
};
}
