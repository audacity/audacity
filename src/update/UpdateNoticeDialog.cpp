
/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file UpdateNoticeDialog.cpp
 @brief Declare a dialog to notify the user about automatic update checking.

 Dmitry Vedenko
 **********************************************************************/

#include "UpdateNoticeDialog.h"

#include <wx/button.h>
#include <wx/stattext.h>

#include "BasicUI.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "CodeConversions.h"
#include "prefs/PrefsDialog.h"
#include "AccessibleLinksFormatter.h"


static const auto windowTitle =
   /* i18n-hint: Window title of the app update notice dialog. */
   XO("Stay Updated & Anonymous Usage Info");

static const auto title =
   /* i18n-hint: First line of the app update notice dialog. */
   XO("App updates");

static const auto firstParagraph =
   /* i18n-hint: The first paragraph of app update notice dialog.*/
   XO("Audacity notifies you in-app when a new version is available to download.");

static const auto subTitle =
   /* i18n-hint: Subtitle of the app update notice dialog. */
   XO("Anonymous usage info (UUID)");

static const auto secondParagraph =
   /* i18n-hint: The second paragraph of app update notice dialog */
   XO("To help us understand how often people use Audacity, we generate a random, anonymous ID (UUID). "
      "This helps us improve features and plan for the future. No personal data is collected.");

static const auto preferencesPage =
   /* i18n-hint: a page in the Preferences dialog; use same name */
   XO("Preferences > Application");

static const auto preferencesPageLinkA =
   /* i18n-hint:  %s is replaced with "Preferences > Application" link */
   XO("You can disable this anytime in %s.");

static const auto preferencesPageLinkB =
   /* i18n-hint:  %s is replaced with "Preferences > Application" link */
   XO("You can turn this off anytime in %s.");

static const std::string privacyPolicyUrl = "https://www.audacityteam.org/about/desktop-privacy-notice/";

enum {
   ViewPrivacyPolicyID = 10000,
   DisableAnonymousInfoID,
};

BEGIN_EVENT_TABLE(UpdateNoticeDialog, wxDialogWrapper)
    EVT_BUTTON(wxID_OK, UpdateNoticeDialog::OnAccept)
    EVT_BUTTON(DisableAnonymousInfoID, UpdateNoticeDialog::OnDecline)
    EVT_BUTTON(ViewPrivacyPolicyID, UpdateNoticeDialog::OnViewPrivacyPolicy)
    EVT_SIZE(UpdateNoticeDialog::OnSize)
END_EVENT_TABLE()

IMPLEMENT_CLASS(UpdateNoticeDialog, wxDialogWrapper)

UpdateNoticeDialog::UpdateNoticeDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, -1, windowTitle, wxDefaultPosition, wxDefaultSize,
         wxCAPTION | wxCLOSE_BOX)
{
   const auto openAppPrefs = [this]() {
      GlobalPrefsDialog dialog(this /* parent */, nullptr);
      dialog.SelectPageByName(XO("Application").Translation());
      dialog.ShowModal();
   };

   ShuttleGui S(this, eIsCreating);

   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxEXPAND, 0);
      {
         S.AddSpace(8);
         S.StartPanel();
         {
            S.SetBorder(5);

            S.AddSpace(15);
            wxStaticText* titleCtrl = S.AddVariableText(title, false, 0, 500);
            wxFont font = titleCtrl->GetFont().MakeLarger().MakeBold();
            titleCtrl->SetFont(font);

            S.AddSpace(10);
            S.AddFixedText(firstParagraph, false, 500);


            AccessibleLinksFormatter preferencesMessageA(preferencesPageLinkA);
            preferencesMessageA.FormatLink(wxT("%s"), preferencesPage, openAppPrefs);
            preferencesMessageA.Populate(S);

            S.AddSpace(20);
            wxStaticText* subtitleCtrl = S.AddVariableText(subTitle, false, 0, 500);
            subtitleCtrl->SetFont(font);

            S.AddSpace(10);
            S.AddFixedText(secondParagraph, false, 500);

            AccessibleLinksFormatter preferencesMessageB(preferencesPageLinkB);
            preferencesMessageB.FormatLink(wxT("%s"), preferencesPage, openAppPrefs);
            preferencesMessageB.Populate(S);
         }
         S.EndPanel();
         S.AddSpace(8);
      }
      S.EndHorizontalLay();

      S.AddSpace(8, 24);

      S.StartHorizontalLay(wxEXPAND);
      {
         S.SetBorder(8);
         S.AddSpace(8, 24);

         S.Id(ViewPrivacyPolicyID).AddButton(XO("View privacy policy"));
         S.AddSpace(1, 1, 1);
         S.Id(DisableAnonymousInfoID).AddButton(XO("Disable UUID"));
         S.Id(wxID_OK).AddButton(XO("Accept && continue"))->SetFocus();

         S.AddSpace(8);
      }
      S.EndHorizontalLay();
      S.AddSpace(8);
   }
   S.EndVerticalLay();

   Fit();
   Layout();

   Center();
}

void UpdateNoticeDialog::OnAccept(wxCommandEvent&)
{
   EndModal(wxOK);
}

void UpdateNoticeDialog::OnViewPrivacyPolicy(wxCommandEvent&)
{
   BasicUI::OpenInDefaultBrowser(privacyPolicyUrl);
}

void UpdateNoticeDialog::OnDecline(wxCommandEvent&)
{
   EndModal(wxNO);
}

void UpdateNoticeDialog::OnSize(wxSizeEvent&)
{
    Fit();
    Layout();
}
