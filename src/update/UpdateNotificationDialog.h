/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateNotificationDialog.h

 **********************************************************************/
#pragma once

#include "UpdateDataStructures.h"
#include "wxPanelWrapper.h"

#include <wx/dialog.h>
#include <wx/string.h>

wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_NOTIFICATION_DISMISSED, wxCommandEvent);
wxDECLARE_EXPORTED_EVENT(AUDACITY_DLL_API, EVT_NOTIFICATION_REMIND_LATER, wxCommandEvent);

class UpdateNotificationDialog final : public wxDialogWrapper
{
public:
    UpdateNotificationDialog(wxWindow* parent, const Notification& notification);
    virtual ~UpdateNotificationDialog();

private:
    void OnDismiss(wxCommandEvent& event);
    void OnRemindLater(wxCommandEvent& event);
    void OnNotificationAction(wxCommandEvent& event);

    const Notification& mNotification;

    DECLARE_EVENT_TABLE()
};
