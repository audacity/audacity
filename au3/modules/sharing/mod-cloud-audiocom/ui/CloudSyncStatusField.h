/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncStatusField.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

// Needed for wx/weakref
#include <type_traits>
#include <wx/weakref.h>

#include "ClientData.h"
#include "Observer.h"

class AudacityProject;

class wxRect;

namespace audacity::cloud::audiocom::sync {
class ProjectCloudExtension;
class CloudStatusChangedMessage;

class CloudSyncStatusField final : public ClientData::Base
{
public:
    explicit CloudSyncStatusField(AudacityProject& project);
    ~CloudSyncStatusField() override;

    static CloudSyncStatusField& Get(AudacityProject& project);
    static const CloudSyncStatusField& Get(const AudacityProject& project);

    int GetWidth() const;
    void OnSize(const wxRect& rect);
    bool IsVisible() const;

    TranslatableString GetText() const;

private:
    class StatusWidget;

    void MarkDirty();
    void OnCloudStatusChanged(const CloudStatusChangedMessage& extension);

    StatusWidget& GetStatusWidget();
    const StatusWidget& GetStatusWidget() const;

    AudacityProject& mProject;
    ProjectCloudExtension& mCloudExtension;

    enum class State
    {
        Hidden,
        Dirty,
        Synced,
        Failed,
        Uploading,
    } mState { State::Hidden };

    int mProgress { 0 }; // Progress, 0-100

    wxWeakRef<StatusWidget> mStatusWidget;

    Observer::Subscription mCloudStatusChangedSubscription;
}; // class CloudSyncStatusField
} // namespace audacity::cloud::audiocom::sync
