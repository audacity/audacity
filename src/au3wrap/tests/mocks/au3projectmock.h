/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "au3wrap/iau3project.h"

namespace au::au3 {
class Au3ProjectMock : public IAu3Project
{
public:
    MOCK_METHOD(muse::Ret, open, (), (override));
    MOCK_METHOD(muse::Ret, load, (const muse::io::path_t& filePath, bool ignoreAutosave), (override));
    MOCK_METHOD(bool, save, (const muse::io::path_t& fileName), (override));
    MOCK_METHOD(void, close, (), (override));

    MOCK_METHOD(std::string, title, (), (const, override));
    MOCK_METHOD(muse::io::path_t, getFileName, (), (const, override));

    MOCK_METHOD(bool, hasUnsavedChanges, (), (const, override));
    MOCK_METHOD(void, markAsSaved, (), (override));
    MOCK_METHOD(bool, isRecovered, (), (const, override));
    MOCK_METHOD(bool, isTemporary, (), (const, override));

    MOCK_METHOD(bool, hasAutosaveData, (), (const, override));
    MOCK_METHOD(muse::Ret, removeAutosaveData, (), (override));

    MOCK_METHOD(muse::async::Notification, projectChanged, (), (const, override));

    MOCK_METHOD(uintptr_t, au3ProjectPtr, (), (const, override));
};

class Au3ProjectCreatorMock : public IAu3ProjectCreator
{
public:
    MOCK_METHOD(std::shared_ptr<IAu3Project>, create, (), (const, override));
    MOCK_METHOD(muse::Ret, removeUnsavedData, (const muse::io::path_t& projectPath), (const, override));
};
}
