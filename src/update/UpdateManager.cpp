/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateManager.cpp
 @brief Declare a class that managing of updates.

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

static const char* prefsUpdatePopupDialogShown = "/Update/UpdatePopupDialogShown";
static const char* prefsUpdateScheduledTime = "/Update/UpdateScheduledTime";

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager::UpdateManager()
    : mTrackingInterval(
        std::chrono::milliseconds(std::chrono::hours(12)).count())
{
    mTimer.SetOwner(this, ID_TIMER);
    mTimer.StartOnce();
}

UpdateManager::~UpdateManager()
{
    mTimer.Stop();
}

void UpdateManager::enableUpdatesChecking(bool enable)
{
    gPrefs->Write(prefsUpdatePopupDialogShown, enable);
    gPrefs->Flush();
}

bool UpdateManager::isUpdatesCheckingEnabled()
{
    return gPrefs->ReadBool(prefsUpdatePopupDialogShown, true);
}

VersionPatch UpdateManager::getVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::getUpdates()
{
    const audacity::network_manager::Request request("https://updates.audacityteam.org/feed/latest.xml");
    auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback([response, this](audacity::network_manager::IResponse*) {

        wxFrame* parent = FindProjectFrame(GetActiveProject());
        wxASSERT(parent);

        if (!parent) return;

        if (response->getError() != audacity::network_manager::NetworkError::NoError)
        {
            ShowExceptionDialog(parent,
                XO("Error checking for update"),
                XO("Unable to connect to Audacity update server."),
                wxString());

            return;
        }

        if (!mUpdateDataParser.Parse(response->readAll<VersionPatch::UpdateDataFormat>(), &mVersionPatch))
        {
            ShowExceptionDialog(parent,
                XO("Error checking for update"),
                XO("Update data was corrupted."),
                wxString());

            return;
        }

        if (mVersionPatch.version > CurrentBuildVersion())
        {
            parent->CallAfter([this, parent] {
                UpdatePopupDialog dlg(parent, this);
                const int code = dlg.ShowModal();

                if (code == wxID_YES)
                {
                    if (!wxLaunchDefaultBrowser(mVersionPatch.download))
                    {
                        ShowExceptionDialog(parent,
                            XO("Error downloading update"),
                            XO("Can't open the Audacity download link."),
                            wxString());

                        return;
                    }
                }
                });
        }
        });
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    if (isUpdatesCheckingEnabled() && isTimeToUpdate())
        getUpdates();

    mTimer.StartOnce(mTrackingInterval);
}

bool UpdateManager::isTimeToUpdate()
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
        nextUpdatesCheckingTime = currentTimeInMillisec + mTrackingInterval;

        gPrefs->Write(prefsUpdateScheduledTime,
            wxString(std::to_string(nextUpdatesCheckingTime)));
        gPrefs->Flush();

        return true;
    }

    return false;
}
