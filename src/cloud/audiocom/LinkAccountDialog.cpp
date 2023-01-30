/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkAccountDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LinkAccountDialog.h"

#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/statline.h>
#include <wx/weakref.h>

#include "ShuttleGui.h"

#include "BasicUI.h"
#include "CodeConversions.h"

#include "OAuthService.h"

#include "AuthorizationHandler.h"
#include "LinkFailedDialog.h"
#include "LinkSucceededDialog.h"

namespace cloud::audiocom
{
LinkAccountDialog::LinkAccountDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Link account"), wxDefaultPosition, { 480, -1 },
         wxDEFAULT_DIALOG_STYLE)
{
   GetAuthorizationHandler().PushSuppressDialogs();

   ShuttleGui s(this, eIsCreating);

   s.StartVerticalLay();
   {
      s.StartInvisiblePanel(16);
      {
         s.SetBorder(0);
         s.AddFixedText(XO("Enter token to link your account"));

         s.AddSpace(0, 4, 0);

         mToken = s.AddTextBox(TranslatableString {}, {}, 60);
         mToken->SetName(XO("Token").Translation());
         mToken->Bind(wxEVT_TEXT, [this](auto) { OnTextChanged(); });

         s.AddSpace(0, 16, 0);

         s.AddWindow(safenew wxStaticLine { s.GetParent() }, wxEXPAND);

         s.AddSpace(0, 10, 0);

         s.StartHorizontalLay(wxEXPAND, 0);
         {
            s.AddSpace(0, 0, 1);

            s.AddButton(XXO("&Cancel"))
               ->Bind(wxEVT_BUTTON, [this](auto) { Close(); });

            mContinueButton = s.AddButton(XXO("C&ontinue"));
            mContinueButton->Disable();
            mContinueButton->Bind(wxEVT_BUTTON, [this](auto) { OnContinue(); });
         }
         s.EndHorizontalLay();
      }
      s.EndInvisiblePanel();
   }
   s.EndVerticalLay();

   Layout();
   Fit();
   Centre();
}

LinkAccountDialog::~LinkAccountDialog()
{
   GetAuthorizationHandler().PopSuppressDialogs();
}

void LinkAccountDialog::OnContinue()
{
   mContinueButton->Disable();

   wxWeakRef<LinkAccountDialog> weakDialog(this);
   
   GetOAuthService().HandleLinkURI(
      audacity::ToUTF8(mToken->GetValue()),
      [weakDialog](auto accessToken)
      {
         BasicUI::CallAfter(
            [weakDialog, token = std::string(accessToken)]()
            {
               if (!token.empty())
               {
                  if (weakDialog)
                  {
                     auto parent = weakDialog->GetParent();
                     weakDialog->Close();

                     LinkSucceededDialog successDialog { parent };
                     successDialog.ShowModal();
                  }

                  return;
               }

               LinkFailedDialog errorDialog(weakDialog);

               if (wxID_RETRY != errorDialog.ShowModal())
               {
                  if (weakDialog)
                     weakDialog->Close();
               }
            });
      });
}

void LinkAccountDialog::OnTextChanged()
{
   mContinueButton->Enable(!mToken->GetValue().empty());
}

} // namespace cloud::audiocom

// Remaining code hooks this add-on into the application
#include "../../commands/CommandContext.h"
#include "../../commands/CommandManager.h"

namespace {
// Define our extra menu item
void OnLinkAccount(const CommandContext&)
{
   cloud::audiocom::LinkAccountDialog dialog;
   dialog.ShowModal();
}

using namespace MenuTable;
AttachedItem sAttachment{
   Placement{ wxT("Help/Extra"), { OrderingHint::Begin } },
      Command(
         wxT("LinkAccount"), XXO("L&ink audio.com account..."),
         OnLinkAccount, AlwaysEnabledFlag) 
};
}
