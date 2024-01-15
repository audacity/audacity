/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  LinkAccountDialog.cpp

  Dmitry Vedenko

**********************************************************************/
#include "LinkAccountDialog.h"

#include <wx/button.h>
#include <wx/stattext.h>
#include <wx/font.h>

#include "CodeConversions.h"
#include "ServiceConfig.h"

#include "ShuttleGui.h"

#include "HelpSystem.h"


namespace cloud::audiocom
{

LinkAccountDialog::LinkAccountDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Link account"), wxDefaultPosition, { 442, -1 },
         wxDEFAULT_DIALOG_STYLE)
{
   SetMinSize({ 442, -1 });

   ShuttleGui s(this, eIsCreating);

   s.StartVerticalLay();
   {
      s.StartInvisiblePanel(16);
      {
         s.SetBorder(0);

         auto header = s.AddVariableText(XO("You are not signed in"), false, 0, 410);
         auto font = header->GetFont();
         font.SetWeight(wxFONTWEIGHT_BOLD);
         font.SetPointSize(static_cast<int>(font.GetPointSize() * 1.5));
         header->SetFont(font);

         s.AddFixedText(XO("Login or create a free account on audio.com to save this project to the cloud"), false, 410);

         s.AddSpace(0, 16, 0);

         s.StartHorizontalLay(wxEXPAND, 0);
         {
            auto cancelBtn = s.AddButton(XO("&Cancel"));
            cancelBtn->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_CANCEL); });

            s.AddSpace(1, 0, 1);

            auto linkBtn = s.AddButton(XO("&Link account"));

            linkBtn->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_OK); });
            linkBtn->SetDefault();
         }
         s.EndHorizontalLay();

      }
      s.EndInvisiblePanel();
   }
   s.EndVerticalLay();

   Layout();
   Fit();
   Center();

   Bind(
      wxEVT_CHAR_HOOK,
      [this](auto& evt)
      {
         if (!IsEscapeKey(evt))
         {
            evt.Skip();
            return;
         }

         EndModal(wxID_CANCEL);
      });
}

LinkAccountDialog::~LinkAccountDialog()
{
}

} // namespace cloud::audiocom
