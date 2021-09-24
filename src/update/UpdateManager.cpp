/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.cpp
 @brief Declare a class that handles managing of updates.

 Anton Gerasimov
 **********************************************************************/

#include "UpdateManager.h"

#include "UpdatePopupDialog.h"
#include "UpdateNoticeDialog.h"
#include "NoUpdatesAvailableDialog.h"

#include "AudioIO.h"
#include "BasicUI.h"
#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

#include <wx/utils.h>
#include <wx/frame.h>
#include <wx/app.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>

#include <cstdint>

#define UPDATE_LOCAL_TESTING 0

static const char* prefsUpdateScheduledTime = "/Update/UpdateScheduledTime";

static BoolSetting
   prefUpdatesNoticeShown(wxT("/Update/UpdateNoticeShown"), false);

using Clock = std::chrono::system_clock;
using TimePoint = Clock::time_point;
using Duration = TimePoint::duration;

#if UPDATE_LOCAL_TESTING == 1
constexpr Duration updatesCheckInterval = std::chrono::minutes(2);
#else
constexpr Duration updatesCheckInterval = std::chrono::hours(12);
#endif

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager& UpdateManager::GetInstance()
{
    static UpdateManager updateManager;

    return updateManager;
}

void UpdateManager::Start()
{
    auto& instance = GetInstance();

    // Show the dialog only once. 
    if (!prefUpdatesNoticeShown.Read())
    {
        // DefaultUpdatesCheckingFlag survives the "Reset Preferences"
        // action, so check, if the updates were previously disabled as well.
        if (DefaultUpdatesCheckingFlag.Read())
        {
            UpdateNoticeDialog notice(nullptr);

            notice.ShowModal();
        }

        prefUpdatesNoticeShown.Write(true);
        gPrefs->Flush();
    }

    static std::once_flag flag;
    std::call_once(flag, [&instance] {
        instance.mTimer.SetOwner(&instance, ID_TIMER);
        instance.mTimer.StartOnce(1);
        });
}

VersionPatch UpdateManager::GetVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::GetUpdates(bool ignoreNetworkErrors, bool configurableNotification)
{
    const audacity::network_manager::Request request("https://updates.audacityteam.org/feed/latest.xml");
    auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback([response, ignoreNetworkErrors, configurableNotification, this](audacity::network_manager::IResponse*) {
        
        // We don't' want to duplicate the updates checking if that already launched.
        {
            std::lock_guard<std::mutex> lock(mUpdateMutex);
            if (mOnProgress)
            {
                response->abort();
                return;
            }
            mOnProgress = true;
        }

        using namespace BasicUI;
        auto gAudioIO = AudioIO::Get();
        if (response->getError() != audacity::network_manager::NetworkError::NoError)
        {
           if (!ignoreNetworkErrors)
           {
              gAudioIO->CallAfterRecording([] {ShowErrorDialog( {},
                 XC("Error checking for update", "update dialog"),
                 XC("Unable to connect to Audacity update server.", "update dialog"),
                 wxString(),
                 ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
              });
           }
           
           mOnProgress = false;
           return;
        }

        if (!mUpdateDataParser.Parse(response->readAll<VersionPatch::UpdateDataFormat>(), &mVersionPatch))
        {
           if (!ignoreNetworkErrors)
           {
              gAudioIO->CallAfterRecording([] {ShowErrorDialog( {},
                 XC("Error checking for update", "update dialog"),
                 XC("Update data was corrupted.", "update dialog"),
                 wxString(),
                 ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
              });
           }
           
           mOnProgress = false;
           return;
        }

#if UPDATE_LOCAL_TESTING == 0
        if (mVersionPatch.version > CurrentBuildVersion())
#endif
        {
            gAudioIO->CallAfterRecording([this, ignoreNetworkErrors, configurableNotification] {
                UpdatePopupDialog dlg(nullptr, mVersionPatch, configurableNotification);
                const int code = dlg.ShowModal();

                if (code == wxID_YES)
                {
                    const audacity::network_manager::Request downloadRequest(mVersionPatch.download.ToStdString());
                    auto downloadResponse = audacity::network_manager::NetworkManager::GetInstance().doGet(downloadRequest);

                    // Called once, when downloading is real will finish.
                    downloadResponse->setRequestFinishedCallback([downloadResponse, ignoreNetworkErrors, this](audacity::network_manager::IResponse*) {
                        // First - close all opened resources.
                        wxTheApp->CallAfter([this]{ mProgressDialog.reset(); });
                        
                        if (mAudacityInstaller.is_open())
                            mAudacityInstaller.close();
                        
                        if (downloadResponse->getError() != audacity::network_manager::NetworkError::NoError)
                        {
                            if (!ignoreNetworkErrors)
                            {
                                wxTheApp->CallAfter([] {ShowErrorDialog( {},
                                  XC("Error downloading update", "update dialog"),
                                  XC("Can't open the Audacity download link.", "update dialog"),
                                  wxString(),
                                  ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
                               });
                            }

                            mOnProgress = false;
                            return;
                        }
                        
                        const wxPlatformInfo& info = wxPlatformInfo::Get();
                        if ((info.GetOperatingSystemId() & wxOS_WINDOWS) ||
                            info.GetOperatingSystemId() & wxOS_MAC)
                        {
                            if (wxFileName(mAudacityInstallerPath).Exists())
                            {
                                std::string cmd = info.GetOperatingSystemId() & wxOS_MAC ? "Open " + mAudacityInstallerPath : mAudacityInstallerPath;
                                wxTheApp->CallAfter([cmd] { wxExecute(cmd, wxEXEC_ASYNC); });
                            }
                        }
                        
                        mOnProgress = false;
                        }
                    );

                    auto audacityPatchFilename = wxFileName(mVersionPatch.download).GetName();

                    mProgressDialog = BasicUI::MakeProgress(XO("Audacity update"), XO("Downloading %s").Format(audacityPatchFilename));
                    wxASSERT(mProgressDialog);
                    
                    wxString installerExtension;
                    const wxPlatformInfo& info = wxPlatformInfo::Get();
                    if (info.GetOperatingSystemId() & wxOS_WINDOWS)
                        installerExtension = "exe";
                    else if(info.GetOperatingSystemId() & wxOS_MAC)
                        installerExtension = "dmg";

                    mAudacityInstallerPath = wxFileName(
                        wxStandardPaths::Get().GetUserDir(wxStandardPaths::Dir_Downloads), audacityPatchFilename, installerExtension)
                        .GetFullPath()
                        .ToStdString();

                    mAudacityInstaller.open(mAudacityInstallerPath, std::ios::binary);
                    
                    // Called each time, since downloading for update progress status.
                    downloadResponse->setDownloadProgressCallback(
                        [this](int64_t current, int64_t expected) {

                        wxTheApp->CallAfter([this, current, expected]{
                            if(mProgressDialog != nullptr)
                                mProgressDialog->Poll(current, expected);
                        });
                    }
                    );

                    // Called each time, since downloading for get data.
                    downloadResponse->setOnDataReceivedCallback([downloadResponse, this](audacity::network_manager::IResponse*) {
                        if (downloadResponse->getError() == audacity::network_manager::NetworkError::NoError)
                        {
                            std::vector<char> buffer(downloadResponse->getBytesAvailable());

                            size_t bytes = downloadResponse->readData(buffer.data(), buffer.size());
                            
                            if (mAudacityInstaller.is_open())
                                mAudacityInstaller.write(buffer.data(), buffer.size());
                        }
                        }
                    );

                }
                else if (code == wxID_NO)
                {
                    mOnProgress = false;
                }
            });
        }
#if UPDATE_LOCAL_TESTING == 0
        else // mVersionPatch.version > CurrentBuildVersion()
        {
            // That also shows, that updates checking was called manually from menu.
            if (!configurableNotification)
            {
                gAudioIO->CallAfterRecording([] {
                    NoUpdatesAvailableDialog(nullptr).ShowModal();
                    });
            }
            
            mOnProgress = false;
        }
#endif
    });
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    bool updatesCheckingEnabled = DefaultUpdatesCheckingFlag.Read();

#if UPDATE_LOCAL_TESTING == 0
    if (updatesCheckingEnabled && IsTimeForUpdatesChecking())
#endif
    {
        GetUpdates(true, true);
    }

    mTimer.StartOnce(std::chrono::duration_cast<std::chrono::milliseconds>(
                        updatesCheckInterval)
                        .count());
}

bool UpdateManager::IsTimeForUpdatesChecking()
{
    // We use atoll here, so there is no need to handle the exception,
    // if prefsUpdateScheduledTime is corrupted.
    // atoll will return 0 on failure, which suits us well.
    const TimePoint nextUpdatesCheckingTime(std::chrono::milliseconds(
       atoll(gPrefs->Read(prefsUpdateScheduledTime, "0").c_str())));

    // Get current time
    const TimePoint currentTime = Clock::now();

    // If next update time 0 or less then current time -> show update dialog,
    // else this condition allow us to avoid from duplicating update notifications.
    if (nextUpdatesCheckingTime < currentTime)
    {

        // Round down the nextUpdatesChecking time to a day.
        // This is required to ensure, that update is 
        // checked daily
        using DayDuration =
          std::chrono::duration<int32_t, std::ratio<60 * 60 * 24>>;

        const auto postponeUpdateUntil =
          std::chrono::time_point_cast<DayDuration>(
           currentTime) + DayDuration(1);

        const std::chrono::milliseconds postponeUpdateUntilMS(
           postponeUpdateUntil.time_since_epoch());

        gPrefs->Write(
           prefsUpdateScheduledTime,
           wxString(std::to_string(postponeUpdateUntilMS.count())));

        gPrefs->Flush();

        return true;
    }

    return false;
}
