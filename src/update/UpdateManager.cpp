/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.cpp
 @brief Declare a class that handles managing of updates.

 Anton Gerasimov
 **********************************************************************/

#include "UpdateManager.h"
#include "UpdatePopupDialog.h"

#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

#include "widgets/ErrorDialog.h"

#include <wx/platinfo.h>
#include <wx/utils.h>
#include <wx/frame.h>

#include <mutex>

BoolSetting UpdatesCheckingSettings::DefaultUpdatesCheckingFlag{
    L"/Update/DefaultUpdatesChecking", true };

static const char* prefsUpdateScheduledTime = "/Update/UpdateScheduledTime";

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager::UpdateManager()
    : mUpdateCheckingInterval(
        std::chrono::milliseconds(std::chrono::hours(12)).count())
{}

UpdateManager::~UpdateManager()
{}

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
}

VersionPatch UpdateManager::GetVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::GetUpdates()
{
    const audacity::network_manager::Request request("https://updates.audacityteam.org/feed/latest.xml");
    auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback([response, this](audacity::network_manager::IResponse*) {

        if (response->getError() != audacity::network_manager::NetworkError::NoError)
        {
            wxTheApp->CallAfter([] {ShowExceptionDialog(nullptr,
                XC("Error checking for update", "update dialog"),
                XC("Unable to connect to Audacity update server.", "update dialog"),
                wxString());
                });

            return;
        }

        if (!mUpdateDataParser.Parse(response->readAll<VersionPatch::UpdateDataFormat>(), &mVersionPatch))
        {
            wxTheApp->CallAfter([] {ShowExceptionDialog(nullptr,
                XC("Error checking for update", "update dialog"),
                XC("Update data was corrupted.", "update dialog"),
                wxString());
                });

            return;
        }

        if (mVersionPatch.version > CurrentBuildVersion())
        {
            wxTheApp->CallAfter([this] {
                UpdatePopupDialog dlg(nullptr, mVersionPatch);
                const int code = dlg.ShowModal();

                if (code == wxID_YES)
                {
                    if (!wxLaunchDefaultBrowser(mVersionPatch.download))
                    {
                        ShowExceptionDialog(nullptr,
                            XC("Error downloading update.", "update dialog"),
                            XC("Can't open the Audacity download link.", "update dialog"),
                            wxString());
                    }
                }
                });
        }
        });
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    bool updatesCheckingEnabled = UpdatesCheckingSettings::DefaultUpdatesCheckingFlag.Read();

    if (updatesCheckingEnabled && IsTimeForUpdatesChecking())
        GetUpdates();

    mTimer.StartOnce(mUpdateCheckingInterval);
}

bool UpdateManager::IsTimeForUpdatesChecking()
{
    long long nextUpdatesCheckingTime = std::stoll(
        gPrefs->Read(prefsUpdateScheduledTime, "0").ToStdString());

    // Get current time in milliseconds
    auto now_ms = std::chrono::time_point_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now());

    auto currentTimeInMillisec = std::chrono::duration_cast<std::chrono::milliseconds>(
        now_ms.time_since_epoch()).count();

    // If next update time 0 or less then current time -> show update dialog,
    // else this condition allow us to avoid from duplicating update notifications.
    if (nextUpdatesCheckingTime < currentTimeInMillisec)
    {
        nextUpdatesCheckingTime = currentTimeInMillisec + mUpdateCheckingInterval;

        gPrefs->Write(prefsUpdateScheduledTime,
            wxString(std::to_string(nextUpdatesCheckingTime)));
        gPrefs->Flush();

        return true;
    }

    return false;
}
