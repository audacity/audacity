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
    
    getUpdates();
    
    mTimer.SetOwner(this, ID_TIMER);
    mTimer.StartOnce(kTimerInterval);
}

UpdateManager::~UpdateManager()
{}

void UpdateManager::enableNotification(bool enable)
{
    gPrefs->Write(prefsUpdatePopupDialogShown, enable);
}

bool UpdateManager::isNotificationEnabled()
{
    return gPrefs->ReadBool(prefsUpdatePopupDialogShown, false);
}

VersionPatch UpdateManager::getVersionPatch() const
{
    return mVersionPatch;
}

void UpdateManager::getUpdates()
{
    ServerCommunication::UpdateDataFormat updateResponse;
    
    ServerCommunication communicator;
    if (!communicator.getUpdateData(&updateResponse))
    {
        // TODO: remake text and may be Parrent.
        AudacityMessageBox(
            XO("Unable to connect on Audacity update server."),
            XO("Error update check"),
            wxOK | wxCENTRE,
            mParent);

        return;
    }
    
    // TODO: check and show MessageBox.
    UpdateDataParser updateDataParser;
    if (!updateDataParser.Parse(updateResponse, &mVersionPatch))
    {
        ;
    }
    
    //if (isNotificationEnabled())
    {
        UpdatePopupDialog dlg(nullptr, this);
        const int code = dlg.ShowModal();
        
        if (code == wxID_YES)
        {
            if (!wxLaunchDefaultBrowser(mVersionPatch.download))
            {
                // TODO: MessageBox with error;
            }
        }
    }
}

void UpdateManager::OnTimer(wxTimerEvent& event)
{
    getUpdates();
    
    mTimer.StartOnce(kTimerInterval);
}
