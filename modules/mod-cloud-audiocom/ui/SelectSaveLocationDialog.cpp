/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SelectSaveLocationDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SelectSaveLocationDialog.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/statline.h>
#include <wx/stattext.h>

#include "ShuttleGui.h"

#include "CloudModuleSettings.h"

namespace cloud::audiocom::sync
{
SelectSaveLocationDialog::SelectSaveLocationDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Audacity"), wxDefaultPosition, { 442, -1 },
         wxDEFAULT_DIALOG_STYLE)
{
   ShuttleGui S(this, eIsCreating);

   wxFont font;

   S.StartVerticalLay();
   {
      S.StartInvisiblePanel(16);
      S.StartVerticalLay();
      {
         S.StartHorizontalLay(wxEXPAND, 0);
         {
            S.AddSpace(0, 0, 1);
            auto title = S.AddVariableText(XO("How would you like to save?"));
            font = title->GetFont();
            font.SetWeight(wxFONTWEIGHT_BOLD);
            font.SetPointSize(font.GetPointSize() + 2);
            title->SetFont(font);
            S.AddSpace(0, 0, 1);
         }
         S.EndHorizontalLay();
         S.StartHorizontalLay();
         {
            S.StartPanel()->SetMinSize(wxSize { 232, -1 });
            {
               S.AddVariableText(XO("Save to the &cloud"))->SetFont(font);
               S.AddSpace(16);
               S.AddFixedText(
                  XO("Your project is backed up privately on audio.com. You can access your work from any device and collaborate on your project with others"),
                  false, 200);

               S.AddSpace(0, 16, 1);

               S.AddButton(XO("Save to cloud"), wxALIGN_LEFT)
                  ->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_SAVE); });
            }
            S.EndPanel();
            S.StartPanel()->SetMinSize(wxSize { 232, -1 });
            {
               S.AddVariableText(XO("On your c&omputer"))->SetFont(font);
               S.AddSpace(16);
               S.AddFixedText(XO("Files are saved on your device"), false, 200);

               S.AddSpace(0, 16, 1);

               S.AddButton(XO("Save to computer"), wxALIGN_LEFT)
                  ->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_OK); });
            }
            S.EndPanel();
         }
         S.EndHorizontalLay();
      }
      S.EndVerticalLay();
      S.EndInvisiblePanel();

      S.AddWindow(safenew wxStaticLine { S.GetParent() }, wxEXPAND);

      S.SetBorder(16);

      auto checkbox = S.AddCheckBox(
         XO("&Remember my choice and don't show again"),
         DoNotShowCloudSyncDialog.Read());

         checkbox->Bind(
         wxEVT_CHECKBOX, [checkbox](auto&)
         {
            DoNotShowCloudSyncDialog.Write(checkbox->GetValue());
            gPrefs->Flush();
         });
   }
   S.EndVerticalLay();

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

SelectSaveLocationDialog::~SelectSaveLocationDialog()
{
}
} // namespace cloud::audiocom::sync
