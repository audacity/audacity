/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudProjectMixdownUtils.cpp

  Dmitry Vedenko

**********************************************************************/
#include "CloudProjectMixdownUtils.h"

#include "BasicUI.h"
#include "CloudProjectFileIOExtensions.h"
#include "CloudProjectOpenUtils.h"
#include "Project.h"
#include "ProjectWindow.h"
#include "ServiceConfig.h"
#include "UriParser.h"

#include "sync/CloudSyncDTO.h"
#include "sync/MixdownUploader.h"
#include "sync/ProjectCloudExtension.h"

namespace audacity::cloud::audiocom::sync {
bool HandleMixdownLink(std::string_view uri)
{
    ASSERT_MAIN_THREAD();

    const auto parsedUri = ParseUri(uri);

    if (parsedUri.Scheme != "audacity" || parsedUri.Host != "generate-audio") {
        return false;
    }

    const auto queryParameters = ParseUriQuery(parsedUri.Query);

    if (queryParameters.empty()) {
        return false;
    }

    const auto projectId = queryParameters.find("projectId");

    if (projectId == queryParameters.end()) {
        return false;
    }

    auto openedProject = GetOpenedProject(projectId->second);

    const auto hasOpenProject = openedProject != nullptr;

    const auto project = hasOpenProject
                         ? openedProject
                         : OpenProjectFromCloud(
        GetPotentialTarget(), projectId->second,
        std::string_view {}, false);

    if (project == nullptr) {
        return false;
    }

    UploadMixdown(
        *project,
        [hasOpenProject](AudacityProject& project, MixdownState state)
    {
        if (!hasOpenProject) {
            ProjectWindow::Get(project).Close(true);
        }
    });

    return true;
}

void UploadMixdown(
    AudacityProject& project,
    std::function<void(AudacityProject&, MixdownState)> onComplete)
{
    SaveToCloud(
        project, UploadMode::Normal,
        [&project, onComplete = std::move(onComplete)](const auto& response)
    {
        auto cancellationContext = concurrency::CancellationContext::Create();

        auto progressDialog = BasicUI::MakeProgress(
            XO("Save to audio.com"), XO("Generating audio preview..."),
            BasicUI::ProgressShowCancel);

        auto mixdownUploader = MixdownUploader::Upload(
            cancellationContext, GetServiceConfig(), project,
            [progressDialog = progressDialog.get(),
             cancellationContext](auto progress)
        {
            if (
                progressDialog->Poll(
                    static_cast<unsigned>(progress * 10000), 10000)
                != BasicUI::ProgressResult::Success) {
                cancellationContext->Cancel();
            }
        });

        mixdownUploader->SetUrls(response.SyncState.MixdownUrls);

        BasicUI::CallAfter(
            [&project,
             progressDialog = std::shared_ptr { std::move(progressDialog) },
             mixdownUploader, cancellationContext, onComplete]() mutable
        {
            auto& projectCloudExtension
                =ProjectCloudExtension::Get(project);

            auto subscription = projectCloudExtension.SubscribeStatusChanged(
                [progressDialog = progressDialog.get(), mixdownUploader,
                 cancellationContext](
                    const CloudStatusChangedMessage& message)
            {
                if (message.Status != ProjectSyncStatus::Failed) {
                    return;
                }

                cancellationContext->Cancel();
            },
                true);

            auto future = mixdownUploader->GetResultFuture();

            while (future.wait_for(std::chrono::milliseconds(50))
                   != std::future_status::ready) {
                BasicUI::Yield();
            }

            auto result = future.get();

            progressDialog.reset();

            if (onComplete) {
                onComplete(project, result.State);
            }
        });
    });
}
} // namespace audacity::cloud::audiocom::sync
