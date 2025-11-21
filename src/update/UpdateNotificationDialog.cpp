/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdateNotificationDialog.cpp

 **********************************************************************/

#include "UpdateNotificationDialog.h"

#include "ShuttleGui.h"
#include "HelpSystem.h"

#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/sizer.h>
#include <wx/utils.h>
#include <wx/html/htmlwin.h>

wxDEFINE_EVENT(EVT_NOTIFICATION_DISMISSED, wxCommandEvent);
wxDEFINE_EVENT(EVT_NOTIFICATION_REMIND_LATER, wxCommandEvent);

enum {
    DismissID = wxID_HIGHEST + 1,
    RemindLaterID,
    notificationActionStartID
};

BEGIN_EVENT_TABLE(UpdateNotificationDialog, wxDialogWrapper)
    EVT_BUTTON(DismissID, UpdateNotificationDialog::OnDismiss)
    EVT_BUTTON(RemindLaterID, UpdateNotificationDialog::OnRemindLater)
    EVT_BUTTON(wxID_OK, UpdateNotificationDialog::OnDismiss)
END_EVENT_TABLE()

UpdateNotificationDialog::UpdateNotificationDialog(wxWindow* parent, const Notification& notification)
    : wxDialogWrapper(parent, -1, Verbatim(notification.title),
        wxDefaultPosition, wxDefaultSize,
        wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
      mNotification(notification)
{
    ShuttleGui S(this, eIsCreating);
    S.SetBorder(10);

    S.StartVerticalLay(wxEXPAND, 1);
    {
        wxString htmlContent = wxT("<html><head>");
        htmlContent += wxT("</head><body>");
        htmlContent += notification.message;
        htmlContent += wxT("</body></html>");

        auto htmlWindow = safenew wxHtmlWindow(S.GetParent(), wxID_ANY,
            wxDefaultPosition, wxSize(400, -1), wxHW_SCROLLBAR_AUTO);
        htmlWindow->SetBorders(10);
        htmlWindow->SetPage(htmlContent);

        wxHtmlContainerCell* cell = htmlWindow->GetInternalRepresentation();
        if (cell)
        {
            cell->Layout(400);
            int height = cell->GetHeight();
            htmlWindow->SetMinSize(wxSize(400, height + 20));
            htmlWindow->SetMaxSize(wxSize(400, 250));
        }

        SetBackgroundColour(htmlWindow->GetBackgroundColour());

        S.AddWindow(htmlWindow, wxEXPAND | wxALL);

        S.AddSpace(10);

        S.StartHorizontalLay(wxALIGN_RIGHT, 0);
        {
            if (!notification.notificationAction.label.IsEmpty())
            {
                auto actionBtn = S.Id(notificationActionStartID).AddButton(Verbatim(notification.notificationAction.label));
                actionBtn->Bind(wxEVT_BUTTON, &UpdateNotificationDialog::OnnotificationAction, this);
                actionBtn->SetDefault();
            }

            S.Id(RemindLaterID).AddButton(XXO("&Remind me later"));

            auto dismissBtn = S.Id(DismissID).AddButton(XXO("&Dismiss"));
            if (notification.notificationAction.label.IsEmpty())
                dismissBtn->SetDefault();
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();

    Layout();
    Fit();
    Center();

    SetMinSize(wxSize(440, -1));
}

UpdateNotificationDialog::~UpdateNotificationDialog()
{
}

void UpdateNotificationDialog::OnDismiss(wxCommandEvent&)
{
    wxCommandEvent event(EVT_NOTIFICATION_DISMISSED, GetId());
    event.SetString(mNotification.uuid);
    ProcessEvent(event);

    EndModal(wxID_OK);
}

void UpdateNotificationDialog::OnRemindLater(wxCommandEvent&)
{
    wxCommandEvent event(EVT_NOTIFICATION_REMIND_LATER, GetId());
    event.SetString(mNotification.uuid);
    ProcessEvent(event);

    EndModal(wxID_CANCEL);
}

void UpdateNotificationDialog::OnnotificationAction(wxCommandEvent&)
{
    if (!mNotification.notificationAction.link.IsEmpty())
    {
        wxLaunchDefaultBrowser(mNotification.notificationAction.link);
    }
}
