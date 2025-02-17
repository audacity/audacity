/**********************************************************************

  Audacity: A Digital Audio Editor

  UnwritableLocationError.h

  Dmitry Vedenko

**********************************************************************/

#include "UnwritableLocationErrorDialog.h"

#include "HelpSystem.h"
#include "ShuttleGui.h"

#include "prefs/PrefsDialog.h"
#include "AccessibleLinksFormatter.h"

BEGIN_EVENT_TABLE(UnwritableLocationErrorDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, UnwritableLocationErrorDialog::OnOk)
EVT_BUTTON(wxID_HELP, UnwritableLocationErrorDialog::OnError)
END_EVENT_TABLE()

UnwritableLocationErrorDialog::UnwritableLocationErrorDialog(wxWindow* parent, const wxString& path)
    : wxDialogWrapper(
        parent, -1, XO("Error"), wxDefaultPosition, wxDefaultSize, wxCAPTION | wxCLOSE_BOX)
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
                S.AddFixedText(
                    /* i18n-hint: %s is replaced with a directory path. */
                    XO("Unable to write files to directory: %s.").Format(path),
                    false, 500);

                S.AddFixedText(
                    /* i18n-hint: This message describes the error in the Error dialog. */
                    XO("Please check that the directory exists, has the necessary permissions, and the drive isn't full."),
                    false, 0);

                S.AddSpace(0, 8);

                AccessibleLinksFormatter preferencesMessage(
                    /* i18n-hint: %s is replaced with 'Preferences > Directories'. */
                    XO("You can change the directory in %s."));

                preferencesMessage.FormatLink(
                    /* i18n-hint: Hyperlink title that opens Preferences dialog on Directories page. */
                    wxT("%s"), XO("Preferences > Directories"), [parent, this]() {
                    EndModal(wxID_OK);

                    GlobalPrefsDialog dialog(parent, nullptr);

                    dialog.SelectPageByName(XO("Directories").Translation());
                    dialog.ShowModal();
                });

                preferencesMessage.Populate(S);
            }
            S.EndVerticalLay();

            S.AddSpace(12, 0);
        }
        S.EndHorizontalLay();

        S.AddSpace(0, 12);

        S.AddStandardButtons(eHelpButton | eOkButton);
    }
    S.EndVerticalLay();

    Layout();
    Fit();
    Center();
}

UnwritableLocationErrorDialog::~UnwritableLocationErrorDialog()
{
}

void UnwritableLocationErrorDialog::OnOk(wxCommandEvent&)
{
    EndModal(wxID_OK);
}

void UnwritableLocationErrorDialog::OnError(wxCommandEvent&)
{
    HelpSystem::ShowHelp(this, "Error:_Disk_full_or_not_writable", false);
}
