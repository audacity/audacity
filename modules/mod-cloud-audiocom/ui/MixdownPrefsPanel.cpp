/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MixdownPrefsPanel.cpp

  Dmitry Vedenko

**********************************************************************/
#include "MixdownPrefsPanel.h"

#include <wx/choice.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>

namespace cloud::audiocom::sync
{
MixdownPrefsPanel::MixdownPrefsPanel(wxWindow* parent, bool compact)
    : wxPanelWrapper { parent }
{
   mRBNever =
      safenew wxRadioButton { this, wxID_ANY, XO("Never").Translation() };
   mRBAlways =
      safenew wxRadioButton { this, wxID_ANY, XO("Always").Translation() };
   mRBEvery =
      safenew wxRadioButton { this, wxID_ANY, XO("Every").Translation() };

   constexpr auto maxFrequency = 30;
   wxArrayString choices;
   choices.reserve(maxFrequency - 2);

   for (auto i = 2; i <= maxFrequency; ++i)
      choices.push_back(XO("%d saves").Format(i).Translation());

   mSavesFrequencyChoice = safenew wxChoice { this, wxID_ANY, wxDefaultPosition,
                                              wxDefaultSize, choices };

   mSavesFrequencyChoice->SetSelection(0);

   auto sizer = safenew wxGridSizer { 1 };

   sizer->Add(mRBNever, wxSizerFlags().CenterVertical());
   sizer->Add(mRBAlways, wxSizerFlags().CenterVertical());

   auto horizontalSizer = safenew wxBoxSizer { wxHORIZONTAL };

   horizontalSizer->Add(mRBEvery, wxSizerFlags().CenterVertical());
   horizontalSizer->Add(
      mSavesFrequencyChoice, wxSizerFlags().Border(wxLEFT, 16));

   sizer->Add(horizontalSizer);

   SetSizer(sizer);

   SetFrequency(0);

   mRBNever->Bind(
      wxEVT_RADIOBUTTON, [this](auto&) { EnableFrequencyChoice(false); });

   mRBAlways->Bind(
      wxEVT_RADIOBUTTON, [this](auto&) { EnableFrequencyChoice(false); });

   mRBEvery->Bind(
      wxEVT_RADIOBUTTON, [this](auto&) { EnableFrequencyChoice(true); });
}

void MixdownPrefsPanel::SetFrequency(int frequency)
{
   assert(frequency >= 0);

   if (frequency < 1)
   {
      mRBNever->SetValue(true);
   }
   else if (frequency == 1)
   {
      mRBAlways->SetValue(true);
   }
   else
   {
      const auto selectedItem = frequency - 2;

      assert(selectedItem < mSavesFrequencyChoice->GetCount());

      mRBEvery->SetValue(true);
      mSavesFrequencyChoice->SetSelection(
         std::min<int>(frequency - 2, mSavesFrequencyChoice->GetCount()));
   }

   EnableFrequencyChoice(frequency >= 2);
}

int MixdownPrefsPanel::GetFrequency() const
{
   if (mRBNever->GetValue())
      return 0;
   else if (mRBAlways->GetValue())
      return 1;
   else
      return mSavesFrequencyChoice->GetSelection() + 2;
}

void MixdownPrefsPanel::EnableFrequencyChoice(bool enable)
{
   mSavesFrequencyChoice->Enable(enable);

   if (enable)
      mSavesFrequencyChoice->MoveAfterInTabOrder(mRBNever);
}

} // namespace cloud::audiocom::sync
