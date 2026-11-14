/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NoUpdatesAvailableDialog.cpp
  @brief Define a dialog with Information about no updates available.

  Anton Gerasimov

**********************************************************************/

#include "NoUpdatesAvailableDialog.h"

#include "HelpSystem.h"
#include "ShuttleGui.h"

#include "prefs/PrefsDialog.h"
#include "AccessibleLinksFormatter.h"

#include <wx/stattext.h>

static const auto title
    =/* i18n-hint: Title of the dialog no updates available. */
      XO("No Updates Available");

BEGIN_EVENT_TABLE(NoUpdatesAvailableDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, NoUpdatesAvailableDialog::OnOk)
END_EVENT_TABLE()

NoUpdatesAvailableDialog::NoUpdatesAvailableDialog(wxWindow* parent)
    : wxDialogWrapper(
        parent, -1, XO("Check for Updates"), wxDefaultPosition, wxDefaultSize, wxCAPTION | wxCLOSE_BOX)
{
    ShuttleGui S(this, eIsCreating);

    S.SetBorder(8);

    S.StartVerticalLay();
    {
        S.AddSpace(0, 12);

        S.StartHorizontalLay();
        {
            S.AddSpace(12, 0);

            S.StartVerticalLay();
            {
                wxStaticText* titleCtrl = S.AddVariableText(title, false, 0, 500);

                wxFont font = titleCtrl->GetFont().MakeLarger().MakeBold();

                titleCtrl->SetFont(font);

                AccessibleLinksFormatter preferencesMessage(
                    /* i18n-hint: %s is replaced with 'Preferences > Application'. */
                    XO("If you want to change your preference for automatic updates checking, you can find it in %s."));

                preferencesMessage.FormatLink(
                    /* i18n-hint: Hyperlink title that opens Preferences dialog on Application page and is substituted into "... you can find it in %s." string. */
                    wxT("%s"), XO("Preferences > Application"), [parent, this]() {
                    EndModal(wxID_OK);

                    GlobalPrefsDialog dialog(parent, nullptr);

                    dialog.SelectPageByName(XO("Application").Translation());
                    dialog.ShowModal();
                });

                preferencesMessage.Populate(S);
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

NoUpdatesAvailableDialog::~NoUpdatesAvailableDialog()
{
}

void NoUpdatesAvailableDialog::OnOk(wxCommandEvent&)
{
    EndModal(wxID_OK);
}
