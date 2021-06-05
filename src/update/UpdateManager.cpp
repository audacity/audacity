#include "UpdateManager.h"
#include "UpdatePopupDialog.h"

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
    wxFrame* parrentFrame = project.GetFrame();
    wxASSERT(parrentFrame);
    
    mParent = reinterpret_cast<wxWindow*> (parrentFrame);
    
    mTimer.SetOwner(this, ID_TIMER);
    mTimer.StartOnce();
}

UpdateManager::~UpdateManager()
{
    mTimer.Stop();
}

void UpdateManager::enableNotification(bool enable)
{
    gPrefs->Write(prefsUpdatePopupDialogShown, enable);
    gPrefs->Flush();
}

bool UpdateManager::isNotificationEnabled()
{
    return gPrefs->ReadBool(prefsUpdatePopupDialogShown, true);
}

VersionPatch UpdateManager::getVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::getUpdates()
{
    ServerCommunication::UpdateDataFormat updateResponse;
    
    ServerCommunication communicator;
    if (!communicator.getUpdateData(updateResponse))
    {
        AudacityMessageBox(
            XO("Unable to connect to Audacity update server."),
            XO("Error checking for update"),
            wxOK | wxCENTRE,
            mParent);

        return;
    }
    
    UpdateDataParser updateDataParser;
    if (!updateDataParser.Parse(updateResponse, &mVersionPatch))
    {
        AudacityMessageBox(
            XO("Update data was corrupted."),
            XO("Error checking for update"),
            wxOK | wxCENTRE,
            mParent);

        return;
    }
    
    if (isNotificationEnabled() && mVersionPatch.version > CurrentBuildVersion())
    {
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
                    mParent);

                return;
            }
        }
    }
}

void UpdateManager::OnTimer(wxTimerEvent& WXUNUSED(event))
{
    getUpdates();
    
    mTimer.StartOnce(
        std::chrono::milliseconds(std::chrono::hours(12)).count());
}
