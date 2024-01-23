/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  SelectSaveLocationDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SelectSaveLocationDialog.h"

#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/sizer.h>
#include <wx/statline.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>

#include "ShuttleGui.h"

#include "CloudModuleSettings.h"

#include "images/CloudImages.hpp"

namespace cloud::audiocom::sync
{
SelectSaveLocationDialog::SelectSaveLocationDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Audacity"), wxDefaultPosition, { 442, -1 },
         wxDEFAULT_DIALOG_STYLE)
{
   wxFont titleFont = GetFont();
   titleFont.SetWeight(wxFONTWEIGHT_BOLD);
   titleFont.SetPointSize(titleFont.GetPointSize() * 2);

   wxFont descriptionFont = GetFont();
   descriptionFont.SetPointSize(descriptionFont.GetPointSize() * 3 / 2);

   auto title = safenew wxStaticText(
      this, wxID_ANY, XO("How would you like to save?").Translation());
   title->SetFont(titleFont);

   auto saveToCloudImage =
      safenew wxStaticBitmap(this, wxID_ANY, *bin2c_SaveRemote_png);

   auto saveToCloudTitle = safenew wxStaticText(
      this, wxID_ANY, XO("Save to the &cloud").Translation());
   saveToCloudTitle->SetFont(titleFont);

   auto saveToCloudDescription = safenew wxStaticText(
      this, wxID_ANY,
      XO("Your project is backed up privately on audio.com. You can access your work from any device and collaborate on your project with others")
         .Translation());
   saveToCloudDescription->SetFont(descriptionFont);
   saveToCloudDescription->Wrap(bin2c_SaveLocally_png->GetWidth() - 32);

   auto saveToCloudButton =
      safenew wxButton(this, wxID_ANY, XXO("Save to &cloud").Translation());

   auto saveToComputerImage =
      safenew wxStaticBitmap(this, wxID_ANY, *bin2c_SaveLocally_png);

   auto saveToComputerTitle = safenew wxStaticText(
      this, wxID_ANY, XO("On your c&omputer").Translation());
   saveToComputerTitle->SetFont(titleFont);

   auto saveToComputerDescription = safenew wxStaticText(
      this, wxID_ANY, XO("Files are saved on your device").Translation());
   saveToComputerDescription->SetFont(descriptionFont);
   saveToComputerDescription->Wrap(bin2c_SaveLocally_png->GetWidth() - 32);

   auto saveToComputerButton =
      safenew wxButton(this, wxID_ANY, XXO("Save to &computer").Translation());

   auto rememberChoiceCheckbox = safenew wxCheckBox(
      this, wxID_ANY,
      XO("&Remember my choice and don't show again").Translation());

   rememberChoiceCheckbox->SetValue(DoNotShowCloudSyncDialog.Read());

   auto sizer = safenew wxBoxSizer(wxVERTICAL);

   sizer->Add(title, wxSizerFlags {}.CenterHorizontal().Border(wxTOP, 16));

   auto topSizer = safenew wxBoxSizer(wxHORIZONTAL);

   auto cloudSizer = safenew wxBoxSizer(wxVERTICAL);

   cloudSizer->Add(saveToCloudImage, wxSizerFlags {});
   cloudSizer->Add(
      saveToCloudTitle, wxSizerFlags {}.Border(wxTOP | wxBOTTOM | wxLEFT, 16));
   cloudSizer->Add(saveToCloudDescription, wxSizerFlags {}.Border(wxLEFT, 16));
   cloudSizer->AddStretchSpacer(1);
   cloudSizer->Add(
      saveToCloudButton, wxSizerFlags {}.Border(wxTOP | wxBOTTOM | wxLEFT, 16));

   topSizer->Add(cloudSizer, wxSizerFlags {}.Expand());

   auto computerSizer = safenew wxBoxSizer(wxVERTICAL);

   computerSizer->Add(saveToComputerImage, wxSizerFlags {});
   computerSizer->Add(
      saveToComputerTitle,
      wxSizerFlags {}.Border(wxTOP | wxBOTTOM | wxLEFT, 16));
   computerSizer->Add(
      saveToComputerDescription, wxSizerFlags {}.Border(wxLEFT, 16));
   computerSizer->AddStretchSpacer(1);
   computerSizer->Add(
      saveToComputerButton,
      wxSizerFlags {}.Border(wxTOP | wxBOTTOM | wxLEFT, 16));

   topSizer->Add(computerSizer, wxSizerFlags {}.Expand());

   sizer->Add(topSizer, wxSizerFlags {}.Expand().Border(wxALL, 16));

   sizer->Add(safenew wxStaticLine(this), wxSizerFlags {}.Expand());

   sizer->Add(rememberChoiceCheckbox, wxSizerFlags {}.Border(wxALL, 16));

   SetSizerAndFit(sizer);

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

   rememberChoiceCheckbox->Bind(
      wxEVT_CHECKBOX,
      [rememberChoiceCheckbox](auto&)
      {
         DoNotShowCloudSyncDialog.Write(rememberChoiceCheckbox->GetValue());
         gPrefs->Flush();
      });

   saveToCloudButton->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_SAVE); });
   saveToComputerButton->Bind(
      wxEVT_BUTTON, [this](auto) { EndModal(wxID_OK); });
}

SelectSaveLocationDialog::~SelectSaveLocationDialog()
{
}
} // namespace cloud::audiocom::sync
