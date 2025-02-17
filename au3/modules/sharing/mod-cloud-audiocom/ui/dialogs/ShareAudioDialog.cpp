/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ShareAudioDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "ShareAudioDialog.h"

#include <cassert>
#include <rapidjson/document.h>

#include <wx/bmpbuttn.h>
#include <wx/button.h>
#include <wx/clipbrd.h>
#include <wx/gauge.h>
#include <wx/frame.h>
#include <wx/stattext.h>
#include <wx/statline.h>
#include <wx/textctrl.h>
#include <wx/radiobut.h>

#include "AllThemeResources.h"
#include "BasicUI.h"
#include "MemoryX.h"
#include "Project.h"
#include "ShuttleGui.h"
#include "Theme.h"
#include "Track.h"
#include "WaveTrack.h"

#include "ServiceConfig.h"
#include "OAuthService.h"
#include "UploadService.h"
#include "UserService.h"

#include "AuthorizationHandler.h"
#include "../UserPanel.h"

#include "CodeConversions.h"

#include "Export.h"
#include "ExportProgressUI.h"
#include "ExportUtils.h"
#include "AccessibleLinksFormatter.h"
#include "ExportPluginRegistry.h"

#include "WindowAccessible.h"
#include "HelpSystem.h"
#include "ProjectRate.h"
#include "ProjectWindows.h"

#include "CloudLocationDialog.h"
#include "ExportUtils.h"

namespace audacity::cloud::audiocom {
namespace {
wxString GenerateTempPath(FileExtension extension)
{
    const auto tempPath = GetUploadTempPath();

    wxFileName fileName(
        tempPath,
        wxString::Format(
            "%lld", std::chrono::system_clock::now().time_since_epoch().count()),
        extension);

    fileName.Mkdir(0700, wxPATH_MKDIR_FULL);

    if (fileName.Exists()) {
        if (!wxRemoveFile(fileName.GetFullPath())) {
            return {}
        }
    }

    return fileName.GetFullPath();
}

const auto publicLabelText = XO("Public");
const auto publicDescriptionText
    =XO("Anyone will be able to listen to this audio.");

const auto unlistedLabelText = XO("Unlisted");
const auto unlistedDescriptionText = XO(
    "Only you and people you share a link with will be able to listen to this audio.");
}

// A helper structures holds UploadService and UploadPromise
struct ShareAudioDialog::Services final
{
    UploadService uploadService;

    UploadOperationHandle uploadPromise;

    Services()
        : uploadService(GetServiceConfig(), GetOAuthService())
    {
    }
};

class ShareAudioDialog::ExportProgressUpdater final : public ExportProcessorDelegate
{
public:
    ExportProgressUpdater(ShareAudioDialog& parent)
        : mParent(parent)
    {
    }

    ~ExportProgressUpdater() override { }

    void Cancel()
    {
        mCancelled.store(true, std::memory_order_release);
    }

    ExportResult GetResult() const
    {
        return mResult;
    }

    void SetResult(ExportResult result)
    {
        mResult = result;
    }

    void SetStatusString(const TranslatableString& str) override
    {
    }

    bool IsCancelled() const override
    {
        return mCancelled.load(std::memory_order_acquire);
    }

    bool IsStopped() const override
    {
        return false;
    }

    void OnProgress(double value) override
    {
        mProgress.store(value, std::memory_order_release);
    }

    void UpdateUI()
    {
        constexpr auto ProgressSteps = 1000ull;

        mParent.UpdateProgress(mProgress.load(std::memory_order_acquire) * ProgressSteps, ProgressSteps);
    }

private:

    ShareAudioDialog& mParent;

    std::atomic<bool> mCancelled{ false };
    std::atomic<double> mProgress;
    ExportResult mResult;
};

ShareAudioDialog::ShareAudioDialog(
    AudacityProject& project, AudiocomTrace trace, wxWindow* parent)
    : wxDialogWrapper(
        parent, wxID_ANY, XO("Share Audio"), wxDefaultPosition, { 480, 250 },
        wxDEFAULT_DIALOG_STYLE)
    , mProject(project)
    , mInitialStatePanel(*this)
    , mServices(std::make_unique<Services>())
    , mAudiocomTrace(trace)
{
    GetAuthorizationHandler().PushSuppressDialogs();

    ShuttleGui s(this, eIsCreating);

    s.StartVerticalLay();
    {
        Populate(s);
    }
    s.EndVerticalLay();

    Layout();
    Fit();
    Centre();

    const auto size = GetSize();

    SetMinSize({ size.x, std::min(250, size.y) });
    SetMaxSize({ size.x, -1 });

    mContinueAction = [this]() {
        if (mInitialStatePanel.root->IsShown()) {
            StartUploadProcess();
        }
    };

    Bind(
        wxEVT_CHAR_HOOK,
        [this](auto& evt)
    {
        if (!IsEscapeKey(evt)) {
            evt.Skip();
            return;
        }

        OnCancel();
    });
}

ShareAudioDialog::~ShareAudioDialog()
{
    GetAuthorizationHandler().PopSuppressDialogs();
    // Clean up the temp file when the dialog is closed
    if (!mFilePath.empty() && wxFileExists(mFilePath)) {
        wxRemoveFile(mFilePath);
    }
}

void ShareAudioDialog::Populate(ShuttleGui& s)
{
    mInitialStatePanel.PopulateInitialStatePanel(s);
    mProgressPanel.PopulateProgressPanel(s);

    s.StartHorizontalLay(wxEXPAND, 0);
    {
        s.StartInvisiblePanel(14);
        {
            s.SetBorder(2);
            s.StartHorizontalLay(wxEXPAND, 0);
            {
                s.AddSpace(0, 0, 1);

                mCancelButton = s.AddButton(XXO("&Cancel"));
                mCancelButton->Bind(wxEVT_BUTTON, [this](auto) { OnCancel(); });

                s.AddSpace(4, 0, 0);

                mContinueButton = s.AddButton(XXO("C&ontinue"));
                mContinueButton->Bind(wxEVT_BUTTON, [this](auto) { OnContinue(); });
            }
            s.EndHorizontalLay();
        }
        s.EndInvisiblePanel();
    }
    s.EndHorizontalLay();

    const auto title = mProject.GetProjectName();

    if (!title.empty()) {
        mInitialStatePanel.trackTitle->SetValue(title);
        mInitialStatePanel.trackTitle->SetInsertionPoint(title.length());
    }

    mContinueButton->Enable(mIsAuthorised && mInitialStatePanel.HasValidTitle());

    mInitialStatePanel.trackTitle->Bind(
        wxEVT_TEXT,
        [this](auto&) {
        mContinueButton->Enable(
            mIsAuthorised && mInitialStatePanel.HasValidTitle());
    });
}

void ShareAudioDialog::OnCancel()
{
    if (mInProgress) {
        AudacityMessageDialog dlgMessage(
            this, XO("Are you sure you want to cancel?"), XO("Cancel upload to Audio.com"),
            wxYES_NO | wxICON_QUESTION | wxNO_DEFAULT | wxSTAY_ON_TOP);

        const auto result = dlgMessage.ShowModal();

        if (result != wxID_YES) {
            return;
        }

        // If export has started, notify it that it should be canceled
        if (mExportProgressUpdater) {
            mExportProgressUpdater->Cancel();
        }
    }

    // If upload was started - ask it to discard the result.
    // The result should be discarded even after the upload has finished
    if (mServices->uploadPromise) {
        mServices->uploadPromise->DiscardResult();
    }

    EndModal(wxID_CANCEL);
}

void ShareAudioDialog::OnContinue()
{
    mContinueAction();
}

namespace {
int CalculateChannels(const TrackList& trackList)
{
    auto range = trackList.Any<const WaveTrack>();
    return std::all_of(range.begin(), range.end(), [](const WaveTrack* track){
        return IsMono(*track) && track->GetPan() == 0;
    }) ? 1 : 2;
}
}

wxString ShareAudioDialog::ExportProject()
{
    auto& tracks = TrackList::Get(mProject);

    const double t0 = 0.0;
    const double t1 = tracks.GetEndTime();

    const int nChannels = CalculateChannels(tracks);

    auto hasMimeType = [](const auto&& mimeTypes, const std::string& mimeType)
    {
        return std::find(mimeTypes.begin(), mimeTypes.end(), mimeType) != mimeTypes.end();
    };

    const auto& registry = ExportPluginRegistry::Get();

    for (const auto& preferredMimeType : GetServiceConfig().GetPreferredAudioFormats()) {
        auto config = GetServiceConfig().GetExportConfig(preferredMimeType);
        ExportProcessor::Parameters parameters;
        auto pluginIt = std::find_if(registry.begin(), registry.end(), [&](auto t)
        {
            auto [plugin, formatIndex] = t;
            parameters.clear();
            return hasMimeType(plugin->GetMimeTypes(formatIndex), preferredMimeType)
                   && plugin->ParseConfig(formatIndex, config, parameters);
        });

        if (pluginIt == registry.end()) {
            continue;
        }

        const auto [plugin, formatIndex] = *pluginIt;

        const auto formatInfo = plugin->GetFormatInfo(formatIndex);
        const auto path = GenerateTempPath(formatInfo.extensions[0]);

        if (path.empty()) {
            continue;
        }

        mExportProgressUpdater = std::make_unique<ExportProgressUpdater>(*this);

        auto builder = ExportTaskBuilder{}
        .SetParameters(parameters)
        .SetNumChannels(nChannels)
        .SetSampleRate(ProjectRate::Get(mProject).GetRate())
        .SetPlugin(plugin)
        .SetFileName(path)
        .SetRange(t0, t1, false);

        auto result = ExportResult::Error;
        ExportProgressUI::ExceptionWrappedCall([&]
        {
            auto exportTask = builder.Build(mProject);

            auto f = exportTask.get_future();
            std::thread(std::move(exportTask), std::ref(*mExportProgressUpdater)).detach();

            ExportProgressUI::ExceptionWrappedCall([&]
            {
                while (f.wait_for(std::chrono::milliseconds(50)) != std::future_status::ready) {
                    mExportProgressUpdater->UpdateUI();
                }
                result = f.get();
            });
        });

        mExportProgressUpdater->SetResult(result);
        const auto success = result == ExportResult::Success;
        if (!success && wxFileExists(path)) {
            wxRemoveFile(path);
        }
        if (success) {
            return path;
        }
    }
    return {};
}

void ShareAudioDialog::StartUploadProcess()
{
    mInProgress = true;

    mInitialStatePanel.root->Hide();
    mProgressPanel.root->Show();

    mProgressPanel.info->Hide();

    mContinueButton->Hide();

    Layout();
    Fit();

    ResetProgress();

    mFilePath = ExportProject();

    if (mFilePath.empty()) {
        if (!mExportProgressUpdater
            || mExportProgressUpdater->GetResult() != ExportResult::Cancelled) {
            HandleExportFailure();
        }

        return;
    }

    mProgressPanel.title->SetLabel(XO("Uploading audio...").Translation());
    ResetProgress();

    mServices->uploadPromise = mServices->uploadService.Upload(
        mFilePath, mInitialStatePanel.GetTrackTitle(), false,
        [this](const auto& result) {
        CallAfter(
            [this, result]()
        {
            mInProgress = false;

            if (result.result == UploadOperationCompleted::Result::Success) {
                // Success indicates that UploadSuccessfulPayload is in the payload
                assert(std::holds_alternative<UploadSuccessfulPayload>(result.payload));

                if (
                    auto payload
                        =std::get_if<UploadSuccessfulPayload>(&result.payload)) {
                    HandleUploadSucceeded(*payload);
                } else {
                    HandleUploadSucceeded({});
                }
            } else if (
                result.result != UploadOperationCompleted::Result::Aborted) {
                if (
                    auto payload
                        =std::get_if<UploadFailedPayload>(&result.payload)) {
                    HandleUploadFailed(*payload);
                } else {
                    HandleUploadFailed({});
                }
            }
        });
    },
        [this](auto current, auto total) {
        CallAfter(
            [this, current, total]()
        {
            UpdateProgress(current, total);
        });
    },
        mAudiocomTrace);
}

void ShareAudioDialog::HandleUploadSucceeded(
    const UploadSuccessfulPayload& payload)
{
    EndModal(wxID_CLOSE);
    OpenInDefaultBrowser(wxString { payload.audioUrl });
}

void ShareAudioDialog::HandleUploadFailed(const UploadFailedPayload& payload)
{
    EndModal(wxID_ABORT);

    TranslatableString message;

    if (!payload.message.empty()) {
        auto details = payload.message;

        for (auto& err : payload.additionalErrors) {
            details += " " + err.second;
        }

        message = XO("Error: %s").Format(details);
    } else {
        message = XO(
            "We are unable to upload this file. Please try again and make sure to link to your audio.com account before uploading.");
    }

    BasicUI::ShowErrorDialog(
        {}, XO("Upload error"),
        message,
        {},
        BasicUI::ErrorDialogOptions { BasicUI::ErrorDialogType::ModalError });
}

void ShareAudioDialog::HandleExportFailure()
{
    EndModal(wxID_ABORT);

    BasicUI::ShowErrorDialog(
        {}, XO("Export error"),
        XO("We are unable to prepare this file for uploading."), {},
        BasicUI::ErrorDialogOptions { BasicUI::ErrorDialogType::ModalError });
}

void ShareAudioDialog::ResetProgress()
{
    mStageStartTime = Clock::now();
    mLastUIUpdateTime = mStageStartTime;

    mProgressPanel.elapsedTime->SetLabel(" 00:00:00");
    mProgressPanel.remainingTime->SetLabel(" 00:00:00");
    mProgressPanel.progress->SetValue(0);

    mLastProgressValue = 0;

    mExportProgressUpdater.reset();

    BasicUI::Yield();
}

namespace {
void SetTimeLabel(wxStaticText* label, std::chrono::milliseconds time)
{
    wxTimeSpan tsElapsed(0, 0, 0, time.count());

    label->SetLabel(tsElapsed.Format(wxT(" %H:%M:%S")));
    label->SetName(label->GetLabel());
    label->Update();
}
}

void ShareAudioDialog::UpdateProgress(uint64_t current, uint64_t total)
{
    using namespace std::chrono;

    const auto now = Clock::now();

    if (current == 0) {
        return;
    }

    if (current > total) {
        current = total;
    }

    if (mLastProgressValue != current) {
        constexpr int scale = 10000;

        mLastProgressValue = static_cast<int>(current);

        mProgressPanel.progress->SetRange(scale);
        mProgressPanel.progress->SetValue((current * scale) / total);

        if (current == total && mServices->uploadPromise) {
            mProgressPanel.timePanel->Hide();
            mProgressPanel.title->SetLabel(XO("Finalizing upload...").Translation());
        }
    }

    const auto elapsedSinceUIUpdate = now - mLastUIUpdateTime;

    constexpr auto uiUpdateTimeout = 500ms;

    if (elapsedSinceUIUpdate < uiUpdateTimeout && current < total) {
        return;
    }

    mLastUIUpdateTime = now;

    const auto elapsed = duration_cast<milliseconds>(now - mStageStartTime);

    SetTimeLabel(mProgressPanel.elapsedTime, elapsed);

    const auto estimate = elapsed * total / current;
    const auto remains = estimate - elapsed;

    SetTimeLabel(
        mProgressPanel.remainingTime,
        std::chrono::duration_cast<std::chrono::milliseconds>(remains));
}

ShareAudioDialog::InitialStatePanel::InitialStatePanel(ShareAudioDialog& parent)
    : parent{parent}
{
}

void ShareAudioDialog::InitialStatePanel::PopulateInitialStatePanel(
    ShuttleGui& s)
{
    root = s.StartInvisiblePanel();
    s.StartVerticalLay(wxEXPAND, 1);
    {
        s.SetBorder(16);

        userPanel = safenew UserPanel { GetServiceConfig(),    GetOAuthService(),
                                        GetUserService(),      UserPanel::LinkMode::Link,
                                        parent.mAudiocomTrace, s.GetParent() };

        mUserDataChangedSubscription = userPanel->Subscribe(
            [this](auto message) { UpdateUserData(message.IsAuthorized); });

        s.Prop(0).AddWindow(userPanel, wxEXPAND);

        s.SetBorder(0);

        s.AddWindow(safenew wxStaticLine { s.GetParent() }, wxEXPAND);

        s.StartInvisiblePanel(16);
        {
            s.StartInvisiblePanel();
            {
                s.AddFixedText(XO("Track Title"));
                s.AddSpace(8);
                trackTitle = s.AddTextBox({}, {}, 60);
                trackTitle->SetName(XO("Track Title").Translation());
                trackTitle->SetFocus();
                trackTitle->SetMaxLength(100);
                s.AddSpace(16);

                anonInfoPanel = s.StartInvisiblePanel();
                {
                    AccessibleLinksFormatter privacyPolicy(XO(
                                                               /*i18n-hint: %s substitutes for audio.com. %% creates a linebreak in this context. */
                                                               "Sharing audio requires a free %s account linked to Audacity. %%Press \"Link account\" above to proceed."));

                    privacyPolicy.FormatLink(
                        L"%s", XO("audio.com"), "https://audio.com");

                    privacyPolicy.FormatLink(
                        L"%%", TranslatableString {},
                        AccessibleLinksFormatter::LinkClickedHandler {});

                    privacyPolicy.Populate(s);
                }
                s.EndInvisiblePanel();

                authorizedInfoPanel = s.StartInvisiblePanel();
                s.StartHorizontalLay(wxEXPAND, 1);
                {
                    s.AddFixedText(XO("Press \"Continue\" to upload to audio.com"));
                }
                s.EndHorizontalLay();
                s.EndInvisiblePanel();
            }
            s.EndInvisiblePanel();
        }
        s.EndInvisiblePanel();
    }
    s.EndVerticalLay();
    s.EndInvisiblePanel();

    UpdateUserData(
        GetOAuthService().HasRefreshToken()
        && !GetUserService().GetUserSlug().empty());
}

void ShareAudioDialog::InitialStatePanel::UpdateUserData(bool authorized)
{
    parent.mIsAuthorised = authorized;

    anonInfoPanel->Show(!authorized);
    authorizedInfoPanel->Show(authorized);

    if (parent.mContinueButton != nullptr) {
        parent.mContinueButton->Enable(authorized && !GetTrackTitle().empty());
    }

    root->GetParent()->Layout();
}

wxString ShareAudioDialog::InitialStatePanel::GetTrackTitle() const
{
    wxString ret { trackTitle->GetValue() };
    ret.Trim(true).Trim(false);
    return ret;
}

bool ShareAudioDialog::InitialStatePanel::HasValidTitle() const
{
    return !GetTrackTitle().empty();
}

void ShareAudioDialog::ProgressPanel::PopulateProgressPanel(ShuttleGui& s)
{
    root = s.StartInvisiblePanel(16);
    root->Hide();
    s.StartVerticalLay(wxEXPAND, 1);
    {
        s.SetBorder(0);

        title = s.AddVariableText(XO("Preparing audio..."));
        s.AddSpace(0, 16, 0);

        progress = safenew wxGauge { s.GetParent(), wxID_ANY, 100 };
        s.AddWindow(progress, wxEXPAND);

        timePanel = s.StartInvisiblePanel();
        {
            s.AddSpace(0, 16, 0);

            s.StartWrapLay();
            {
                s.AddFixedText(XO("Elapsed Time:"));
                elapsedTime = s.AddVariableText(Verbatim(" 00:00:00"));
            }
            s.EndWrapLay();

            s.StartWrapLay();
            {
                s.AddFixedText(XO("Remaining Time:"));
                remainingTime = s.AddVariableText(Verbatim(" 00:00:00"));
            }
            s.EndWrapLay();
        }
        s.EndInvisiblePanel();

        s.AddSpace(0, 16, 0);

        info = s.AddVariableText(publicDescriptionText);
    }

    s.EndVerticalLay();
    s.EndInvisiblePanel();

    wxFont font = elapsedTime->GetFont();
    font.MakeBold();

    elapsedTime->SetFont(font);
    remainingTime->SetFont(font);
}

namespace {
auto hooked = [] {
    ExportUtils::RegisterExportHook(
        [](AudacityProject& project, const FileExtension&, AudiocomTrace trace,
           bool selectedOnly) {
        if (selectedOnly) {
            return ExportUtils::ExportHookResult::Continue;
        }

        const auto window = &GetProjectFrame(project);

        sync::CloudLocationDialog locationDialog {
            window, sync::LocationDialogType::Export
        };

        const auto result = locationDialog.ShowDialog();

        if (result == sync::LocationDialogResult::Cancel) {
            return ExportUtils::ExportHookResult::Cancel;
        }

        if (result == sync::LocationDialogResult::Local) {
            return ExportUtils::ExportHookResult::Continue;
        }

        ShareAudioDialog shareDialog { project, trace, window };
        shareDialog.ShowModal();

        return ExportUtils::ExportHookResult::Handled;
    },
        1000);
    return true;
}();
} // namespace
} // namespace audacity::cloud::audiocom
