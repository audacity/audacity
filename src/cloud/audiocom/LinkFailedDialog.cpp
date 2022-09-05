/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkFailedDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LinkFailedDialog.h"

#include <wx/button.h>

#include "CodeConversions.h"
#include "ServiceConfig.h"

#include "ShuttleGui.h"

#include "widgets/HelpSystem.h"


namespace cloud::audiocom
{

LinkFailedDialog::LinkFailedDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("AccountError"), wxDefaultPosition, { 442, -1 },
         wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui s(this, eIsCreating);

   s.StartVerticalLay();
   {
      s.StartInvisiblePanel(16);
      {
         s.SetBorder(0);

         s.AddFixedText(
            XO("There has been a problem with verifying your linked account. Please visit audio.com to confirm your details and try again."),
            false, 410);

         s.AddSpace(0, 16, 0);

         s.StartHorizontalLay(wxEXPAND, 0);
         {
            s.AddSpace(1, 0, 1);

            s.AddButton(XO("&Go to audio.com"))
               ->Bind(
                  wxEVT_BUTTON,
                  [this](auto)
                  {
                     OpenInDefaultBrowser({ audacity::ToWXString(
                        GetServiceConfig().GetOAuthLoginPage()) });
                     EndModal(wxID_RETRY);
                  });

            s.AddButton(XO("&Close"))
               ->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_ABORT); });
         }
         s.EndHorizontalLay();
         
      }
      s.EndInvisiblePanel();
   }
   s.EndVerticalLay();

   Layout();
   Fit();
}

LinkFailedDialog::~LinkFailedDialog()
{
}

} // namespace cloud::audiocom
