/**********************************************************************

  Audacity: A Digital Audio Editor

  MissingPluginsDialogError.h

**********************************************************************/

#include <wx/artprov.h>
#include <wx/statbmp.h>

#include "AccessibleLinksFormatter.h"
#include "MissingPluginsErrorDialog.h"
#include "ShuttleGui.h"
#include "Theme.h"

BEGIN_EVENT_TABLE(MissingPluginsErrorDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, MissingPluginsErrorDialog::OnOk)
END_EVENT_TABLE()

MissingPluginsErrorDialog::MissingPluginsErrorDialog(wxWindow* parent)
    : wxDialogWrapper(
        parent, -1, XO("Missing Plugins"), wxDefaultPosition, wxDefaultSize, wxCAPTION | wxCLOSE_BOX)
{
    ShuttleGui S(this, eIsCreating);

    S.SetBorder(8);

    S.StartVerticalLay();
    {
        S.AddSpace(0, 12);

        S.StartHorizontalLay();
        {
            S.AddSpace(12, 0);

            wxBitmap bitmap = wxArtProvider::GetBitmap(wxART_WARNING, wxART_MESSAGE_BOX, wxSize(24, 24));

            S.Prop(0).AddWindow(safenew wxStaticBitmap(S.GetParent(), -1, bitmap));

            S.AddSpace(12, 0);

            S.StartVerticalLay();
            {
                S.AddFixedText(
                    XO("This project contains some realtime effect plugins that cannot be found on this system."),
                    false, 500);

                AccessibleLinksFormatter pluginsMessage(
                    /* i18n-hint: %s will be replaced with "Learn more" */
                    XO("The project may sound different than intended. %s"));

                pluginsMessage.FormatLink(
                    /* i18n-hint: Title of hyperlink to audacityteam.org/errors. */
                    wxT("%s"),
                    XO("Learn more"),
                    "https://audacityteam.org/errors#missing-plugins");

                pluginsMessage.Populate(S);
            }
            S.EndVerticalLay();

            S.AddSpace(12, 0);
        }
        S.EndHorizontalLay();

        S.AddSpace(0, 12);

        S.AddStandardButtons(eOkButton);
    }
    S.EndVerticalLay();

    Layout();
    Fit();
    Center();
}

MissingPluginsErrorDialog::~MissingPluginsErrorDialog()
{
}

void MissingPluginsErrorDialog::OnOk(wxCommandEvent&)
{
    EndModal(wxID_OK);
}
