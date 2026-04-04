/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "project/iprojectfilescontroller.h"

namespace au::project {
class ProjectFilesControllerMock : public IProjectFilesController
{
public:
    MOCK_METHOD(bool, isUrlSupported, (const QUrl&), (const, override));
    MOCK_METHOD(bool, isFileSupported, (const muse::io::path_t&), (const, override));
    MOCK_METHOD(muse::Ret, openProject, (const ProjectFile&), (override));
    MOCK_METHOD(bool, closeOpenedProject, (bool), (override));
    MOCK_METHOD(bool, saveProject, (const muse::io::path_t&), (override));
    MOCK_METHOD(bool, saveProjectLocally, (const muse::io::path_t&, SaveMode), (override));
    MOCK_METHOD(bool, saveProjectToCloud, (const CloudProjectInfo&, SaveMode, bool), (override));
    MOCK_METHOD(const ProjectBeingDownloaded&, projectBeingDownloaded, (), (const, override));
    MOCK_METHOD(muse::async::Notification, projectBeingDownloadedChanged, (), (const, override));
};
}
