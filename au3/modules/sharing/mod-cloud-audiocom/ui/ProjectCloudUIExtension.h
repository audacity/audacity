/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ProjectCloudUIExtension.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <atomic>

#include "ClientData.h"
#include "Observer.h"

class AudacityProject;

namespace BasicUI {
class ProgressDialog;
}

namespace audacity::cloud::audiocom::sync {
struct CloudStatusChangedMessage;

class ProjectCloudUIExtension final : public ClientData::Base
{
public:
    explicit ProjectCloudUIExtension(AudacityProject& project);
    ~ProjectCloudUIExtension() override;

    static ProjectCloudUIExtension& Get(AudacityProject& project);
    static const ProjectCloudUIExtension& Get(const AudacityProject& project);

    bool AllowClosing();

private:
    void SetUploadProgress(double progress);
    void OnCloudStatusChanged(const CloudStatusChangedMessage& message);

    AudacityProject& mProject;

    std::unique_ptr<BasicUI::ProgressDialog> mProgressDialog;

    double mProgress { 0.0 };

    std::atomic<bool> mInSync { false };
    bool mClosingCancelled { false };

    Observer::Subscription mCloudStatusChangedSubscription;
};
} // namespace audacity::cloud::audiocom::sync
