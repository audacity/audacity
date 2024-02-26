/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownPropertiesDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MixdownPropertiesDialog.h"

#include <wx/sizer.h>
#include <wx/stattext.h>

#include "CloudModuleSettings.h"
#include "CloudSettings.h"

#include "../MixdownPrefsPanel.h"
#include "Internat.h"

namespace audacity::cloud::audiocom::sync
{
MixdownPropertiesDialog::MixdownPropertiesDialog(wxWindow* parent)
    : wxDialogWrapper { parent, wxID_ANY, XO("Save to audio.com") }
{
   auto header = new wxStaticText {
      this, wxID_ANY,
      XO("Generate a mixdown for audio.com playback").Translation()
   };

   auto font = header->GetFont();
   font.SetWeight(wxFONTWEIGHT_BOLD);
   font.SetPointSize(static_cast<int>(font.GetPointSize() * 1.5));
   header->SetFont(font);

   auto description1 = new wxStaticText {
      this, wxID_ANY,
      XO("The first time you save a project we generate a mixdown so it can be previewed on audio.com.")
         .Translation()
   };

   auto description2 = new wxStaticText {
      this, wxID_ANY,
      XO("Please select how often "
         "you'd like to generate a new mixdown of your project. This setting can be changed in the preferences menu.")
         .Translation()
   };

   description1->Wrap(400);
   description2->Wrap(400);

   mMixdownPrefsPanel = new MixdownPrefsPanel { this, true };

   auto sizer = new wxBoxSizer { wxVERTICAL };

   sizer->Add(header, wxSizerFlags().Border(wxALL, 16));
   sizer->Add(
      description1, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));
   sizer->Add(
      description2, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));
   sizer->Add(
      mMixdownPrefsPanel,
      wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));

   auto buttons = CreateStdDialogButtonSizer(wxOK);
   sizer->Add(
      buttons, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16).Right());

   SetSizerAndFit(sizer);
   Centre(wxBOTH);
}

MixdownPropertiesDialog::~MixdownPropertiesDialog()
{
}

void MixdownPropertiesDialog::Show(wxWindow* parent)
{
   MixdownPropertiesDialog dialog { parent };
   dialog.SetFrequency(MixdownGenerationFrequency.Read());
   dialog.ShowModal();

   const auto frequency = dialog.GetFrequency();
   MixdownGenerationFrequency.Write(frequency);
   gPrefs->Flush();
}

void MixdownPropertiesDialog::ShowIfNeeded(wxWindow* parent)
{
   if (MixdownDialogShown.Read())
      return;

   MixdownPropertiesDialog::Show(parent);
   MixdownDialogShown.Write(true);
}

void MixdownPropertiesDialog::SetFrequency(int frequency)
{
   mMixdownPrefsPanel->SetFrequency(frequency);
}

int MixdownPropertiesDialog::GetFrequency() const
{
   return mMixdownPrefsPanel->GetFrequency();
}
} // namespace audacity::cloud::audiocom::sync
