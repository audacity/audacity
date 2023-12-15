/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownPropertiesDialog.cpp

  Dmitry Vedenko

**********************************************************************/

#include "MixdownPropertiesDialog.h"

#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

#include "CloudModuleSettings.h"
#include "Internat.h"


namespace cloud::audiocom::sync
{
MixdownPropertiesDialog::MixdownPropertiesDialog(wxWindow* parent)
    : wxDialogWrapper { parent, wxID_ANY, XO("Save to audio.com") }
{
   mRBNever = new wxRadioButton { this, wxID_ANY, XO("Never").Translation() };
   mRBAlways = new wxRadioButton { this, wxID_ANY, XO("Always").Translation() };
   mRBEvery = new wxRadioButton { this, wxID_ANY, XO("Every").Translation() };

   constexpr auto maxFrequency = 30;
   wxArrayString choices;
   choices.reserve(maxFrequency - 2);

   for (auto i = 2; i <= maxFrequency; ++i)
      choices.push_back(XO("%d saves").Format(i).Translation());

   mSavesFrequencyChoice = new wxChoice { this, wxID_ANY, wxDefaultPosition,
                                          wxDefaultSize, choices };

   mSavesFrequencyChoice->SetSelection(0);

   auto header = new wxStaticText { this, wxID_ANY, XO("Generate a mixdown for audio.com playback").Translation() };

   auto font = header->GetFont();
   font.SetWeight(wxFONTWEIGHT_BOLD);
   font.SetPointSize(static_cast<int>(font.GetPointSize() * 1.5));
   header->SetFont(font);

   auto description1 = new wxStaticText
   {
      this, wxID_ANY,
         XO("The first time you save a project we generate a mixdown so it  can be previewed on audio.com.")
            .Translation()
   };

   auto description2 = new wxStaticText
   {
      this, wxID_ANY,
      XO("Please select how often"
         "you'd like to generate a new mixdown of your project. This setting can be changed in the preferences menu.")
            .Translation()
   };

   description1->Wrap(400);
   description2->Wrap(400);


   auto sizer = new wxBoxSizer { wxVERTICAL };

   sizer->Add(header, wxSizerFlags().Border(wxALL, 16));
   sizer->Add(description1, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));
   sizer->Add(description2, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));

   sizer->Add(mRBNever, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));
   sizer->Add(mRBAlways, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));

   auto horizontalSizer = new wxBoxSizer { wxHORIZONTAL };

   horizontalSizer->Add(mRBEvery, wxSizerFlags().CenterVertical());
   horizontalSizer->Add(
      mSavesFrequencyChoice, wxSizerFlags().Border(wxLEFT, 16));

   sizer->Add(
      horizontalSizer, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16));

   auto buttons = CreateStdDialogButtonSizer(wxOK);
   sizer->Add(buttons, wxSizerFlags().Border(wxLEFT | wxRIGHT | wxBOTTOM, 16).Right());

   SetSizerAndFit(sizer);
   Centre(wxBOTH);
}

MixdownPropertiesDialog::~MixdownPropertiesDialog()
{
}

int MixdownPropertiesDialog::Show(wxWindow* parent)
{
   MixdownPropertiesDialog dialog { parent };
   dialog.SetFrequency(MixdownGenerationFrequency.Read());
   dialog.ShowModal();

   const auto frequency = dialog.GetFrequency();
   MixdownGenerationFrequency.Write(frequency);
   gPrefs->Flush();

   return frequency;
}

void MixdownPropertiesDialog::SetFrequency(int frequency)
{
   if (frequency == 0)
   {
      mRBNever->SetValue(true);
   }
   else if (frequency == 1)
   {
      mRBAlways->SetValue(true);
   }
   else
   {
      mRBEvery->SetValue(true);
      mSavesFrequencyChoice->SetSelection(frequency - 2);
   }
}
int MixdownPropertiesDialog::GetFrequency() const
{
   if (mRBNever->GetValue())
      return 0;
   else if (mRBAlways->GetValue())
      return 1;
   else
      return mSavesFrequencyChoice->GetSelection() + 2;
}
} // namespace cloud::audiocom::sync
