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
#include <wx/statbmp.h>
#include <wx/statline.h>
#include <wx/stattext.h>

#include "CloudModuleSettings.h"

#include "../images/CloudImages.hpp"

namespace audacity::cloud::audiocom::sync
{
namespace
{
/* i18n-hint: A title that is shown on the first project save that allows the
 * user to select Cloud or local save. */
const auto TitleText = XO("How would you like to save?");

const auto CloudSaveTitle = XO("Save to the Cloud");

const auto CloudSaveDescription = XO(
   "Your project is backed up privately on audio.com. You can access your work from any device and collaborate on your project with others");

const auto CloudSaveButton = XXO("&Save to Cloud");

const auto LocalSaveTitle = XO("On your computer");

const auto LocalSaveDescription = XO("Files are saved on your device");

const auto LocalSaveButton = XXO("Save to &computer");


constexpr auto leftPadding = 16;

auto GetWrapWidth()
{
   return bin2c_SaveLocally_png->GetWidth() - leftPadding * 2;
}

std::unique_ptr<wxBoxSizer> SetupVerticalSizer(
   wxStaticBitmap* image, wxStaticText* title, wxStaticText* description,
   wxButton* button)
{
   const auto leftPaddingFlags = wxSizerFlags {}.Border(wxLEFT, leftPadding);

   auto sizer = std::make_unique<wxBoxSizer>(wxVERTICAL);

   sizer->Add(image);
   sizer->AddSpacer(24);
   sizer->Add(title, leftPaddingFlags);
   sizer->AddSpacer(12);
   sizer->Add(description, leftPaddingFlags);
   sizer->AddSpacer(40);
   sizer->AddStretchSpacer(1);
   sizer->Add(button, leftPaddingFlags);

   return sizer;
}

wxButton* CreateButton(wxWindow* parent, const wxFont& font, const TranslatableString& label)
{
   const auto transalatedLabel = label.Translation();

   auto button =
      safenew wxButton(parent, wxID_ANY, transalatedLabel);

   button->SetFont(font);

   const auto textSize = button->GetTextExtent(transalatedLabel);

   button->SetMinSize({ textSize.x + 12 * 2, 32 });
   
   return button;
}

} // namespace

SelectSaveLocationDialog::SelectSaveLocationDialog(wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Audacity"), wxDefaultPosition, { 442, -1 },
         wxDEFAULT_DIALOG_STYLE)
{
   wxFont titleFont = GetFont();
   titleFont.SetWeight(wxFONTWEIGHT_BOLD);
   titleFont.SetPixelSize({ 0, 18 });

   wxFont descriptionFont = GetFont();
   descriptionFont.SetPixelSize({ 0, 14 });

   auto title = safenew wxStaticText(this, wxID_ANY, TitleText.Translation());
   title->SetFont(titleFont);

   auto saveToCloudImage =
      safenew wxStaticBitmap(this, wxID_ANY, *bin2c_SaveRemote_png);

   auto saveToCloudTitle =
      safenew wxStaticText(this, wxID_ANY, CloudSaveTitle.Translation());
   saveToCloudTitle->SetFont(titleFont);

   auto saveToCloudDescription =
      safenew wxStaticText(this, wxID_ANY, CloudSaveDescription.Translation());
   saveToCloudDescription->SetFont(descriptionFont);
   saveToCloudDescription->Wrap(GetWrapWidth());

   auto saveToCloudButton = CreateButton(this, descriptionFont, CloudSaveButton);

   auto saveToComputerImage =
      safenew wxStaticBitmap(this, wxID_ANY, *bin2c_SaveLocally_png);

   auto saveToComputerTitle = safenew wxStaticText(
      this, wxID_ANY, LocalSaveTitle.Translation());
   saveToComputerTitle->SetFont(titleFont);

   auto saveToComputerDescription = safenew wxStaticText(
      this, wxID_ANY, LocalSaveDescription.Translation());
   saveToComputerDescription->SetFont(descriptionFont);
   saveToComputerDescription->Wrap(GetWrapWidth());

   auto saveToComputerButton =
      CreateButton(this, descriptionFont, LocalSaveButton);

   auto rememberChoiceCheckbox = safenew wxCheckBox(
      this, wxID_ANY,
      XO("&Remember my choice and don't show again").Translation());

   rememberChoiceCheckbox->SetValue(!ShowCloudSyncDialog.Read());

   auto sizer = safenew wxBoxSizer(wxVERTICAL);

   sizer->Add(title, wxSizerFlags {}.CenterHorizontal().Border(wxTOP, 16));

   auto topSizer = safenew wxBoxSizer(wxHORIZONTAL);

   auto cloudSizer = SetupVerticalSizer(
      saveToCloudImage, saveToCloudTitle, saveToCloudDescription,
      saveToCloudButton);

   topSizer->Add(cloudSizer.release(), wxSizerFlags {}.Expand());

   auto computerSizer = SetupVerticalSizer(
      saveToComputerImage, saveToComputerTitle, saveToComputerDescription,
      saveToComputerButton);

   topSizer->Add(
      computerSizer.release(), wxSizerFlags {}.Expand().Border(wxLEFT, 8));

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
         ShowCloudSyncDialog.Write(!rememberChoiceCheckbox->GetValue());
         gPrefs->Flush();
      });

   saveToCloudButton->Bind(wxEVT_BUTTON, [this](auto) { EndModal(wxID_SAVE); });
   saveToComputerButton->Bind(
      wxEVT_BUTTON, [this](auto) { EndModal(wxID_OK); });
}

SelectSaveLocationDialog::~SelectSaveLocationDialog()
{
}

SaveLocationDialogResult SelectSaveLocationDialog::ShowDialog()
{
   const auto result = ShowModal();

   if (result == wxID_OK)
      return SaveLocationDialogResult::Local;

   if (result == wxID_SAVE)
      return SaveLocationDialogResult::Cloud;

   return SaveLocationDialogResult::Cancel;
}
} // namespace audacity::cloud::audiocom::sync
