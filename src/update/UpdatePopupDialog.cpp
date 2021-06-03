/*!********************************************************************
 Audacity: A Digital Audio Editor

 @file UpdatePopupDialog.cpp
 @brief Define a dialog for notifying users about new version available.

 Anton Gerasimov
 **********************************************************************/

#include "update/UpdatePopupDialog.h"

#include "ShuttleGui.h"
#include "widgets/HelpSystem.h"

#include <wx/debug.h>
#include <wx/sstream.h>
#include <wx/txtstrm.h>

enum { DontShowID = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE (UpdatePopupDialog, wxDialogWrapper)
    EVT_BUTTON (wxID_YES, UpdatePopupDialog::OnUpdate)
    EVT_BUTTON (wxID_NO, UpdatePopupDialog::OnSkip)
    EVT_CHECKBOX (DontShowID, UpdatePopupDialog::OnDontShow)
END_EVENT_TABLE()

IMPLEMENT_CLASS (UpdatePopupDialog, wxDialogWrapper)

UpdatePopupDialog::UpdatePopupDialog (wxWindow* parent, UpdateManager* updateManager)
/* i18n-hint: information about the anonymous data collection */
    : wxDialogWrapper (parent, -1, XO ("Update Audacity"),
        wxDefaultPosition, wxDefaultSize,
        wxCAPTION),
      mUpdateManager (updateManager)
{
    ShuttleGui S (this, eIsCreating);
    S.SetBorder (5);
    S.StartVerticalLay (wxEXPAND, 1);
    {
        S.AddWindow (AddHtmlContent (S.GetParent()));

        S.StartHorizontalLay (wxEXPAND, 0);
        {
            S.SetBorder (5);
            // TODO: replace false on mUpdateManager->isNotificationEnabled();
            S.Id (DontShowID).AddCheckBox (XO ("Don't show this again at start up"), false);
            //TODO: ALIG buttons to Right side.
            S.Id (wxID_NO).AddButton (XO ("Skip"));
            S.Id (wxID_YES).AddButton (XO ("Install update"));
            S.SetBorder (5);
        }
        S.EndHorizontalLay();
    }
    S.EndVerticalLay();

    Layout();
    Fit();
    Center();
}

UpdatePopupDialog::~UpdatePopupDialog()
{
    ;
}

void UpdatePopupDialog::OnUpdate (wxCommandEvent&)
{
    EndModal (wxID_YES);
}

void UpdatePopupDialog::OnSkip (wxCommandEvent&)
{
    EndModal (wxID_NO);
    //Show (false);
}

void UpdatePopupDialog::OnDontShow (wxCommandEvent& event)
{
    mUpdateManager->enableNotification (!event.IsChecked());
    //gPrefs->Flush();
}

HtmlWindow* UpdatePopupDialog::AddHtmlContent (wxWindow* parent)
{
    wxStringOutputStream o;
    wxTextOutputStream informationStr (o);

    const auto title = wxString ("Audacity ")
        + mUpdateManager->getVersionPatch().version.getString()
        + " is available!";

    informationStr
        << wxT("<html><body><h3>")
        << title
        << wxT("</h3><h5>")
        << wxString("Changelog")
        << wxT("</h5><p>");

    informationStr << wxT("<ul>");
    for (auto& logLine : mUpdateManager->getVersionPatch().changelog)
    {
        informationStr << wxT("<li>");
        informationStr << logLine;
        informationStr << wxT("</li>");
    }
    informationStr << wxT("</ul></p>");

    informationStr << wxT("<p>");
    informationStr << "<a href = \"https://github.com/audacity/audacity/releases\">Read more om github</a>";
    informationStr << wxT("</p>");

    informationStr << wxT("</body></html>");

    HtmlWindow* html = safenew LinkingHtmlWindow (parent, -1,
        wxDefaultPosition,
        wxSize (500, -1),
        wxHW_SCROLLBAR_NEVER | wxNO_BORDER | wxHW_NO_SELECTION);

    html->SetBorders (20);
    html->SetPage (o.GetString());

    wxHtmlContainerCell* cell = html->GetInternalRepresentation();

    cell->Layout (500);

    const wxSize size = wxSize (500, cell->GetHeight());

    html->SetMinSize (size);
    html->SetMaxSize (size);
    html->SetSize (size);

    return html;
}
