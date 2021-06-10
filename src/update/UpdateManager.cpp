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

#include "widgets/AudacityMessageBox.h"

#include <wx/platinfo.h>
#include <wx/utils.h>

static const char* prefsUpdatePopupDialogShown = "/Update/UpdatePopupDialogShown";
static const char* prefsUpdateScheduledTime = "/Update/UpdateScheduledTime";

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager::UpdateManager(AudacityProject& project)
    : mTrackingInterval(
        std::chrono::milliseconds(std::chrono::hours(12)).count())
{
    mParent = (wxWindow*)(&GetProjectFrame(project));
    wxASSERT(mParent);

    mTimer.SetOwner(this, ID_TIMER);
    mTimer.StartOnce();
}

UpdateManager::~UpdateManager()
{
    mTimer.Stop();
}

void UpdateManager::enableTracking(bool enable)
{
    gPrefs->Write(prefsUpdatePopupDialogShown, enable);
    gPrefs->Flush();
}

bool UpdateManager::isTrackingEnabled()
{
    return gPrefs->ReadBool(prefsUpdatePopupDialogShown, true);
}

VersionPatch UpdateManager::getVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::getUpdates()
{
    audacity::network_manager::Request request("https://updates.audacityteam.org/feed/latest.xml");
    auto response = audacity::network_manager::NetworkManager::GetInstance().doGet(request);

    response->setRequestFinishedCallback([response, this](audacity::network_manager::IResponse*) {

        if (response->getError() != audacity::network_manager::NetworkError::NoError)
        {
            AudacityMessageBox(
                XO("Unable to connect to Audacity update server."),
                XO("Error checking for update"),
                wxOK | wxCENTRE,
                NULL);

            return;
        }

        if (!mUpdateDataParser.Parse(response->readAll<VersionPatch::UpdateDataFormat>(), &mVersionPatch))
        {
            AudacityMessageBox(
                XO("Update data was corrupted."),
                XO("Error checking for update"),
                wxOK | wxCENTRE,
                NULL);

            return;
        }

        if (mVersionPatch.version > CurrentBuildVersion())
        {
            mParent->CallAfter([this] {
                UpdatePopupDialog dlg(mParent, this);
                const int code = dlg.ShowModal();

                if (code == wxID_YES)
                {
                    if (!wxLaunchDefaultBrowser(mVersionPatch.download))
                    {
                        AudacityMessageBox(
                            XO("Can't open the Audacity download link."),
                            XO("Error downloading update"),
                            wxOK | wxCENTRE,
                            NULL);

                        return;
                    }
                }
                });
        }
        });
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    if (isTrackingEnabled() && isTimeToUpdate())
        getUpdates();

    mTimer.StartOnce(mTrackingInterval);
}

bool UpdateManager::isTimeToUpdate()
{
    long long nextTrackingTime = std::stoll(
        gPrefs->Read(prefsUpdateScheduledTime, "0").ToStdString());

    // Get current time in milliseconds
    auto now_ms = std::chrono::time_point_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now());

    auto currentTimeInMillisec = std::chrono::duration_cast<std::chrono::milliseconds>(
        now_ms.time_since_epoch()).count();

    // If next update time 0 or less then current time -> show update dialog,
    // else this condition allow us to avoid from duplicating update notifications.
    if (nextTrackingTime < currentTimeInMillisec)
    {
        nextTrackingTime = currentTimeInMillisec + mTrackingInterval;

        gPrefs->Write(prefsUpdateScheduledTime,
            wxString(std::to_string(nextTrackingTime)));
        gPrefs->Flush();

        return true;
    }

    return false;
}
