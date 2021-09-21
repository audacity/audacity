/**********************************************************************

  Audacity: A Digital Audio Editor

  NoUpdatesAvailableDialog.cpp

  Anton Gerasimov

**********************************************************************/

#include "NoUpdatesAvailableDialog.h"

#include "widgets/HelpSystem.h"
#include "ShuttleGui.h"

#include "prefs/PrefsDialog.h"
#include "ui/AccessibleLinksFormatter.h"

#include <wx/stattext.h>

static const auto title =
/* i18n-hint: Title of the dialog no updates available. */
XO("No updates available");

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
               XO("You are on the latest version of Audacity. Consider turning %s on in Preferences, if you want to be informed automatically about new versions."));

            preferencesMessage.FormatLink(
               /* i18n-hint: Hyperlink title that opens Preferences dialog on Directories page. */
               wxT("%s"), XO("Update Notifications"), [parent, this]() {
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
    ;
}

void NoUpdatesAvailableDialog::OnOk(wxCommandEvent&)
{
   EndModal(wxID_OK);
}
