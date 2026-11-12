/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "project/irecentfilescontroller.h"

namespace au::project {
class RecentFilesControllerMock : public IRecentFilesController
{
public:
    MOCK_METHOD(const RecentFilesList&, recentFilesList, (), (const, override));
    MOCK_METHOD(muse::async::Notification, recentFilesListChanged, (), (const, override));
    MOCK_METHOD(void, prependRecentFile, (const RecentFile&), (override));
    MOCK_METHOD(void, moveRecentFile, (const muse::io::path_t&, const RecentFile&), (override));
    MOCK_METHOD(void, clearRecentFiles, (), (override));
};
}
