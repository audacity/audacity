/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.cpp
 @brief Declare a class that handles managing of updates.

 Anton Gerasimov
 **********************************************************************/

#include "UpdateManager.h"

#include "UpdatePopupDialog.h"
#include "UpdateNoticeDialog.h"

#include "AudioIO.h"
#include "BasicUI.h"
#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

#include <wx/utils.h>
#include <wx/frame.h>
#include <wx/app.h>

#include <mutex>
#include <cstdint>

static const char* prefsUpdateScheduledTime = "/Update/UpdateScheduledTime";

static BoolSetting
   prefUpdatesNoticeShown(wxT("/Update/UpdateNoticeShown"), false);


using Clock = std::chrono::system_clock;
using TimePoint = Clock::time_point;
using Duration = TimePoint::duration;

constexpr Duration updatesCheckInterval = std::chrono::hours(12);

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

    static std::once_flag flag;

    std::call_once(flag, [&instance] {
        instance.mTimer.SetOwner(&instance, ID_TIMER);
        instance.mTimer.StartOnce(1);
        });

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
}

VersionPatch UpdateManager::GetVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::GetUpdates(bool ignoreNetworkErrors)
{
    const audacity::network_manager::Request request("https://updates.audacityteam.org/feed/latest.xml");
    auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback([response, ignoreNetworkErrors, this](audacity::network_manager::IResponse*) {

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
           
           return;
        }

        if (mVersionPatch.version > CurrentBuildVersion())
        {
            gAudioIO->CallAfterRecording([this] {
                UpdatePopupDialog dlg(nullptr, mVersionPatch);
                const int code = dlg.ShowModal();

                if (code == wxID_YES)
                {
                    if (!wxLaunchDefaultBrowser(mVersionPatch.download))
                    {
                        ShowErrorDialog( {},
                            XC("Error downloading update.", "update dialog"),
                            XC("Can't open the Audacity download link.", "update dialog"),
                            wxString(),
                            ErrorDialogOptions{ ErrorDialogType::ModalErrorReport });
                    }
                }
            });
        }
    });
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    bool updatesCheckingEnabled = DefaultUpdatesCheckingFlag.Read();

    if (updatesCheckingEnabled && IsTimeForUpdatesChecking())
        GetUpdates(true);

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
