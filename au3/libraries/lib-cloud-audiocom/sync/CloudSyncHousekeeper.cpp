/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncHousekeeper.cpp

  Dmitry Vedenko

**********************************************************************/

#include <algorithm>
#include <atomic>
#include <chrono>
#include <future>

#include <wx/file.h>

#include "AppEvents.h"
#include "CodeConversions.h"

#include "CloudLibrarySettings.h"
#include "CloudProjectsDatabase.h"

namespace audacity::cloud::audiocom::sync {
namespace {
class Housekeeper final
{
public:
    Housekeeper()
    {
        AppEvents::OnAppInitialized([this] { OnAppInitialized(); });
        AppEvents::OnAppClosing([this] { OnAppClosing(); });
    }

private:
    void OnAppInitialized()
    {
        mHousekeepingOperation = std::async([this] { PerformHousekeeping(); });
    }

    void PerformHousekeeping()
    {
        const auto timeToKeep = std::chrono::hours(24 * DaysToKeepFiles.Read());
        const auto now        = std::chrono::system_clock::now();

        auto& cloudProjectsDatabase = CloudProjectsDatabase::Get();

        auto projects = cloudProjectsDatabase.GetCloudProjects();

        for (const auto& project : projects) {
            if (mHousekeepingCancelled.load()) {
                return;
            }

            const auto path = ToWXString(project.LocalPath);

            if (!wxFileExists(path)) {
                cloudProjectsDatabase.DeleteProject(project.ProjectId);
                continue;
            }

            const auto lastAccess
                =std::max(project.LastModified, project.LastRead);

            const auto discardTreshold
                =std::chrono::system_clock::from_time_t(lastAccess) + timeToKeep;

            if (discardTreshold > now) {
                continue;
            }

            if (wxRemoveFile(path)) {
                cloudProjectsDatabase.DeleteProject(project.ProjectId);
            }
        }

        // Do we need to remove the files that are not in the database?
    }

    void OnAppClosing()
    {
        mHousekeepingCancelled.store(true);
        if (mHousekeepingOperation.valid()) {
            mHousekeepingOperation.wait();
        }

        auto& cloudProjectsDatabase = CloudProjectsDatabase::Get();

        auto connectionLock = cloudProjectsDatabase.GetConnection();

        if (!connectionLock) {
            return;
        }

        auto vacuumStatement = connectionLock->CreateStatement("VACUUM");

        if (!vacuumStatement) {
            return;
        }

        vacuumStatement->Prepare().Run();
    }

    std::future<void> mHousekeepingOperation;
    std::atomic<bool> mHousekeepingCancelled { false };
}; // class Housekeeper

static Housekeeper housekeeper;
} // namespace
} // namespace audacity::cloud::audiocom::sync
