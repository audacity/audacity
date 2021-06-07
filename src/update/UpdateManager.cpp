#include "UpdateManager.h"
#include "UpdatePopupDialog.h"

#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

#include "widgets/AudacityMessageBox.h"

#include <wx/platinfo.h>
#include <wx/utils.h>

static const char* prefsUpdatePopupDialogShown = "/Update/UpdatePopupDialogShown";

enum { ID_TIMER = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(UpdateManager, wxEvtHandler)
    EVT_TIMER(ID_TIMER, UpdateManager::OnTimer)
END_EVENT_TABLE()

UpdateManager::UpdateManager(AudacityProject& project)
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
    if (!isTrackingEnabled())
        return;

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
    getUpdates();

    mTimer.StartOnce(
        std::chrono::milliseconds(std::chrono::hours(12)).count());
}
