/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file UpdateNoticeDialog.cpp
 @brief Declare a dialog to notify the user about automatic update checking.

 Dmitry Vedenko
 **********************************************************************/

#include "UpdateNoticeDialog.h"

#include <wx/button.h>
#include <wx/stattext.h>

#include "ShuttleGui.h"
#include "CodeConversions.h"
#include "prefs/PrefsDialog.h"
#include "AccessibleLinksFormatter.h"

static const auto title
    =/* i18n-hint: Title of the app update notice dialog. */
      XO("App update checking");

static const auto firstParagraph
    =/* i18n-hint: The first paragraph of app update notice dialog. */
      XO("To stay up to date, you will receive an in-app notification whenever there is a new version of Audacity available to download.");

static const auto secondParagraph
    =/* i18n-hint: The second paragraph of app update notice dialog */
      XO(
          "In order to protect your privacy, Audacity does not collect any personal information. However, app update checking does require network access.");

static const auto thirdParagraph
    =/* i18n-hint: Hint to the user about how to turn the app update off. %s is replaced with "Preferences > Application" link*/
      XO("You can turn off app update checking at any time in %s.");

BEGIN_EVENT_TABLE(UpdateNoticeDialog, wxDialogWrapper)
EVT_BUTTON(wxID_OK, UpdateNoticeDialog::OnOk)
EVT_SIZE(UpdateNoticeDialog::OnSize)
END_EVENT_TABLE()

IMPLEMENT_CLASS(UpdateNoticeDialog, wxDialogWrapper)

UpdateNoticeDialog::UpdateNoticeDialog(wxWindow* parent)
    : wxDialogWrapper(
        /* i18n-hint: Title of the app update notice dialog. */
        parent, -1, XO("App updates"), wxDefaultPosition, wxDefaultSize,
        wxCAPTION | wxCLOSE_BOX)
{
    ShuttleGui S(this, eIsCreating);

    S.StartVerticalLay();
    {
        S.AddSpace(0, 16);

        S.StartHorizontalLay(wxEXPAND, 0);
        {
            S.AddSpace(24, 0);

            S.StartPanel();
            {
                S.SetBorder(8);

                wxStaticText* titleCtrl = S.AddVariableText(title, false, 0, 500);

                wxFont font = titleCtrl->GetFont().MakeLarger().MakeBold();

                titleCtrl->SetFont(font);

                S.AddFixedText(firstParagraph, false, 500);

                S.AddFixedText(secondParagraph, false, 500);

                S.AddSpace(0, 8);

                /* i18n-hint: %s will be replaced with "our Privacy Policy" */
                AccessibleLinksFormatter privacyPolicy(XO("See %s for more info."));

                privacyPolicy.FormatLink(
                    /* i18n-hint: Title of hyperlink to the privacy policy. This is an object of "See". */
                    wxT("%s"), XO("our Privacy Policy"),
                    "https://www.audacityteam.org/about/desktop-privacy-notice/");

                privacyPolicy.Populate(S);

                AccessibleLinksFormatter preferencesMessage(thirdParagraph);

                preferencesMessage.FormatLink(
                    // i18n-hint: a page in the Preferences dialog; use same name
                    wxT("%s"), XO("Preferences > Application"), [this]() {
                    GlobalPrefsDialog dialog(this /* parent */, nullptr);

                    dialog.SelectPageByName(XO("Application").Translation());
                    dialog.ShowModal();
                });

                preferencesMessage.Populate(S);
            }
            S.EndPanel();

            S.AddSpace(24, 0);
        }
        S.EndHorizontalLay();

        S.StartHorizontalLay(wxEXPAND);
        {
            S.AddSpace(1, 0, 1);

            S.Id(wxID_OK).AddButton(XO("&OK"))->SetFocus();

            S.AddSpace(8, 0);
        }
        S.EndHorizontalLay();
    }

    S.EndVerticalLay();

    Fit();
    Layout();

    Center();
}

void UpdateNoticeDialog::OnOk(wxCommandEvent&)
{
    EndModal(wxOK);
}

void UpdateNoticeDialog::OnSize(wxSizeEvent&)
{
    Fit();
    Layout();
}
