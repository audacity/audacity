/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ShareAudioDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <chrono>
#include <memory>

#include "wxPanelWrapper.h"
#include "Prefs.h"
#include "Observer.h"

class AudacityProject;

class ShuttleGui;

class wxBitmapButton;
class wxButton;
class wxGauge;
class wxPanel;
class wxStaticText;
class wxTextCtrl;
class wxRadioButton;
enum class AudiocomTrace;

namespace audacity::cloud::audiocom {
class UserPanel;

struct UploadFailedPayload;
struct UploadSuccessfulPayload;

class ShareAudioDialog final : public wxDialogWrapper
{
public:
    ShareAudioDialog(
        AudacityProject& project, AudiocomTrace, wxWindow* parent = nullptr);
    ~ShareAudioDialog() override;

private:
    void Populate(ShuttleGui& s);

    void OnCancel();
    void OnContinue();

    wxString ExportProject();

    void StartUploadProcess();
    void HandleUploadSucceeded(const UploadSuccessfulPayload& payload);
    void HandleUploadFailed(const UploadFailedPayload& payload);
    void HandleExportFailure();

    void ResetProgress();
    void UpdateProgress(uint64_t current, uint64_t total);

    AudacityProject& mProject;

    struct InitialStatePanel final
    {
        explicit InitialStatePanel(ShareAudioDialog& parent);

        ShareAudioDialog& parent;

        wxWindow* root { nullptr };

        UserPanel* userPanel { nullptr };
        wxPanel* anonInfoPanel { nullptr };
        wxPanel* authorizedInfoPanel { nullptr };
        wxTextCtrl* trackTitle { nullptr };

        Observer::Subscription mUserDataChangedSubscription;

        void PopulateInitialStatePanel(ShuttleGui& s);

        void UpdateUserData(bool authorized);

        wxString GetTrackTitle() const;
        bool HasValidTitle() const;
    } mInitialStatePanel;

    struct ProgressPanel final
    {
        wxWindow* root { nullptr };

        wxStaticText* title { nullptr };
        wxGauge* progress { nullptr };

        wxWindow* timePanel { nullptr };
        wxStaticText* elapsedTime { nullptr };
        wxStaticText* remainingTime { nullptr };

        wxStaticText* info { nullptr };

        void PopulateProgressPanel(ShuttleGui& s);
    } mProgressPanel;

    wxButton* mContinueButton { nullptr };
    wxButton* mCancelButton { nullptr };

    struct Services;
    std::unique_ptr<Services> mServices;
    const AudiocomTrace mAudiocomTrace;

    class ExportProgressUpdater;
    std::unique_ptr<ExportProgressUpdater> mExportProgressUpdater;

    using Clock = std::chrono::steady_clock;

    Clock::time_point mStageStartTime;
    Clock::time_point mLastUIUpdateTime;
    int mLastProgressValue { 0 };

    wxString mFilePath;

    std::function<void()> mContinueAction;

    bool mIsAuthorised { false };
    bool mInProgress { false };
};
} // namespace audacity::cloud::audiocom
