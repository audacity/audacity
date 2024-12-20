/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "project/iaudacityproject.h"

namespace au::project {
class AudacityProjectMock : public IAudacityProject
{
public:
    MOCK_METHOD(muse::Ret, createNew, (), (override));
    MOCK_METHOD(muse::Ret, load, (const muse::io::path_t& path, bool forceMode, const std::string& format), (override));

    MOCK_METHOD(void, close, (), (override));
    MOCK_METHOD(muse::async::Notification, aboutCloseBegin, (), (const, override));
    MOCK_METHOD(muse::async::Notification, aboutCloseEnd, (), (const, override));

    MOCK_METHOD(bool, isNewlyCreated, (), (const, override));
    MOCK_METHOD(bool, isImported, (), (const, override));

    MOCK_METHOD(muse::String, title, (), (const, override));

    MOCK_METHOD(QString, displayName, (), (const, override));
    MOCK_METHOD(muse::async::Notification, displayNameChanged, (), (const, override));
    MOCK_METHOD(muse::io::path_t, path, (), (const, override));
    MOCK_METHOD(muse::async::Notification, pathChanged, (), (const, override));
    MOCK_METHOD(muse::ValNt<bool>, needSave, (), (const, override));
    MOCK_METHOD(muse::Ret, canSave, (), (const, override));
    MOCK_METHOD(bool, needAutoSave, (), (const, override));
    MOCK_METHOD(void, setNeedAutoSave, (bool val), (override));
    MOCK_METHOD(muse::async::Notification, needSaveChanged, (), (const, override));
    MOCK_METHOD(muse::Ret, save, (const muse::io::path_t& path, SaveMode saveMode), (override));

    MOCK_METHOD(const au::trackedit::ITrackeditProjectPtr, trackeditProject, (), (const, override));

    MOCK_METHOD(projectscene::IProjectViewStatePtr, viewState, (), (const, override));

    MOCK_METHOD(uintptr_t, au3ProjectPtr, (), (const, override));
};
}
