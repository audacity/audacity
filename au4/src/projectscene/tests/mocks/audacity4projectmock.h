#pragma once

#include <gmock/gmock.h>

#include "../project/iaudacityproject.h"

namespace au::projectscene {
class Audacity4ProjectMock : public project::IAudacityProject
{
public:
    MOCK_METHOD(uintptr_t, au3ProjectPtr, (), (const, override));
    MOCK_METHOD(muse::Ret, load, (const muse::io::path_t&, bool, const std::string&), (override));
    MOCK_METHOD(void, close, (), (override));
    MOCK_METHOD(muse::async::Notification, aboutCloseBegin, (), (const, override));
    MOCK_METHOD(muse::async::Notification, aboutCloseEnd, (), (const, override));
    MOCK_METHOD(bool, isNewlyCreated, (), (const, override));
    MOCK_METHOD(bool, isImported, (), (const, override));
    MOCK_METHOD(QString, displayName, (), (const, override));
    MOCK_METHOD(muse::io::path_t, path, (), (const, override));
    MOCK_METHOD(muse::async::Notification, pathChanged, (), (const, override));
    MOCK_METHOD(muse::ValNt<bool>, needSave, (), (const, override));
    MOCK_METHOD(muse::Ret, canSave, (), (const, override));
    MOCK_METHOD(bool, needAutoSave, (), (const, override));
    MOCK_METHOD(void, setNeedAutoSave, (bool), (override));
    MOCK_METHOD(muse::Ret, save, (const muse::io::path_t&, project::SaveMode), (override));
    MOCK_METHOD(muse::async::Notification, captureThumbnailRequested, (), (const, override));
    MOCK_METHOD(const au::trackedit::ITrackeditProjectPtr, trackeditProject, (), (const, override));
    MOCK_METHOD(projectscene::IProjectViewStatePtr, viewState, (), (const, override));

private:
    void createAu3Project();
};
}
