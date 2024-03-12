/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file PitchAndSpeedDialog.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "PitchAndSpeedDialog.h"
#include "TimeAndPitchInterface.h"
#include "TimeStretching.h"
#include "WaveClipUIUtilities.h"

#include <wx/button.h>
#include <wx/layout.h>
#include <wx/spinctrl.h>
#include <wx/textctrl.h>

#include "ShuttleGui.h"
#include "SpinControl.h"
#include "WaveClip.h"
#include "wxWidgetsWindowPlacement.h"

#include <regex>

namespace
{
template <typename ReturnType, typename... Args> class ScopedSizer
{
public:
   ScopedSizer(
      ShuttleGui& s, ReturnType (ShuttleGui::*startFunc)(Args...),
      void (ShuttleGui::*endFunc)(), Args... args)
       : mShuttleGui(s)
       , mStartFunction(startFunc)
       , mEndFunction(endFunc)
   {
      (mShuttleGui.*mStartFunction)(std::forward<Args>(args)...);
   }

   ~ScopedSizer()
   {
      (mShuttleGui.*mEndFunction)();
   }

private:
   ShuttleGui& mShuttleGui;
   ReturnType (ShuttleGui::*mStartFunction)(Args...);
   void (ShuttleGui::*mEndFunction)();
};

class ScopedHorizontalLay : public ScopedSizer<void, int, int>
{
public:
   ScopedHorizontalLay(
      ShuttleGui& s, int PositionFlags = wxALIGN_CENTRE, int iProp = 1)
       : ScopedSizer<void, int, int>(
            s, &ShuttleGui::StartHorizontalLay, &ShuttleGui::EndHorizontalLay,
            PositionFlags, iProp)
   {
   }
};

class ScopedInvisiblePanel : public ScopedSizer<wxPanel*, int>
{
public:
   ScopedInvisiblePanel(ShuttleGui& s, int border = 0)
       : ScopedSizer<wxPanel*, int>(
            s, &ShuttleGui::StartInvisiblePanel, &ShuttleGui::EndInvisiblePanel,
            border)
   {
   }
};

class ScopedVerticalLay : public ScopedSizer<void, int>
{
public:
   ScopedVerticalLay(ShuttleGui& s, int iProp = 1)
       : ScopedSizer<void, int>(
            s, &ShuttleGui::StartVerticalLay, &ShuttleGui::EndVerticalLay,
            iProp)
   {
   }
};

class ScopedStatic :
    public ScopedSizer<wxStaticBox*, const TranslatableString&, int>
{
public:
   ScopedStatic(ShuttleGui& s, const TranslatableString& label, int iProp = 0)
       : ScopedSizer<wxStaticBox*, const TranslatableString&, int>(
            s, &ShuttleGui::StartStatic, &ShuttleGui::EndStatic, label, iProp)
   {
   }
};

//! Returns true if and only if `output` was updated.
auto GetInt(const wxCommandEvent& event, int& output)
{
   try
   {
      const auto str = event.GetString().ToStdString();
      if (str.empty() || str == "-")
      {
         output = 0;
         return true;
      }
      // Exact integer match
      if (!std::regex_match(str, std::regex { "^-?[0-9]+$" }))
         return false;
      output = std::stoi(str);
      return true;
   }
   catch (const std::exception&)
   {
      return false;
   }
}
} // namespace

PitchAndSpeedDialog::PitchAndSpeedDialog(
   std::weak_ptr<AudacityProject> project, bool playbackOngoing,
   WaveTrack& waveTrack, WaveTrack::Interval& interval, wxWindow* parent,
   const std::optional<PitchAndSpeedDialogFocus>& focus)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Pitch and Speed"), wxDefaultPosition,
         { 480, 250 }, wxDEFAULT_DIALOG_STYLE)
    , mProject { project }
    , mPlaybackOngoing { playbackOngoing }
    , mTrack { waveTrack }
    , mTrackInterval { interval }
    , mClipSpeed { 100.0 / interval.GetStretchRatio() }
    , mOldClipSpeed { mClipSpeed }
{
   const auto totalShift = mTrackInterval.GetCentShift();
   mShift.cents = totalShift % 100;
   mShift.semis = (totalShift - mShift.cents) / 100;
   mOldShift = mShift;

   ShuttleGui s(this, eIsCreating);

   {
      ScopedVerticalLay v { s };
      PopulateOrExchange(s, focus);
   }

   // TODO: Tolerance?
   // Stretch ratio of
   assert(mOldClipSpeed > 0.0);

   Layout();
   Fit();
   Centre();

   Bind(wxEVT_CHAR_HOOK, [this](auto& evt) {
      if (!IsEscapeKey(evt))
      {
         evt.Skip();
         return;
      }

      OnCancel();
   });
}

PitchAndSpeedDialog::~PitchAndSpeedDialog() = default;

void PitchAndSpeedDialog::PopulateOrExchange(
   ShuttleGui& s, const std::optional<PitchAndSpeedDialogFocus>& focus)
{
   {
      ScopedInvisiblePanel panel { s, 15 };
      s.SetBorder(0);
      {
         ScopedStatic scopedStatic { s, XO("Clip Pitch") };
         {
            ScopedHorizontalLay h { s, wxLeft };
            s.SetBorder(2);
            // Use `TieSpinCtrl` rather than `AddSpinCtrl`, too see updates
            // instantly when `UpdateDialog` is called.
            s.TieSpinCtrl(
                XO("semitones:"), mShift.semis,
                TimeAndPitchInterface::MaxCents / 100,
                TimeAndPitchInterface::MinCents / 100)
               ->Bind(wxEVT_TEXT, [&](wxCommandEvent& event) {
                  const auto prevSemis = mShift.semis;
                  if (GetInt(event, mShift.semis))
                  {
                     // If we have e.g. -3 semi, -1 cents, and the user changes
                     // the sign of the semitones, the logic in
                     // `OnPitchShiftChange` would result in 2 semi, 99 cents.
                     // If the user changes sign again, we would now get 1 semi,
                     // -1 cents. Mirrorring (e.g. -3 semi, -1 cents -> 3 semi,
                     // 1 cents) is not a good idea because that would ruin the
                     // work of users painstakingly adjusting the cents of an
                     // instrument. So instead, we map -3 semi, -1 cents to 3
                     // semi, 99 cents.
                     if (mShift.cents != 0)
                     {
                        if (prevSemis < 0 && mShift.semis > 0)
                           ++mShift.semis;
                        else if (prevSemis > 0 && mShift.semis < 0)
                           --mShift.semis;
                     }
                     OnPitchShiftChange(true);
                  }
                  else
                     // Something silly was entered; reset dialog
                     UpdateDialog();
               });
            s.TieSpinCtrl(XO("cents:"), mShift.cents, 100, -100)
               ->Bind(wxEVT_TEXT, [&](wxCommandEvent& event) {
                  if (GetInt(event, mShift.cents))
                     OnPitchShiftChange(false);
                  else
                     // Something silly was entered; reset dialog
                     UpdateDialog();
               });
         }
      }

      s.AddSpace(0, 12);
      s.SetBorder(0);

      {
         ScopedStatic scopedStatic { s, XO("Clip Speed") };
         {
            ScopedHorizontalLay h { s, wxLeft };
            auto txtCtrl = s.Name(XO("Clip Speed"))
                              .NameSuffix(Verbatim("%"))
                              .TieNumericTextBox({}, mClipSpeed, 14, true);
            txtCtrl->Enable(!mPlaybackOngoing);
            if (focus.has_value() && *focus == PitchAndSpeedDialogFocus::Speed)
               txtCtrl->SetFocus();
            if (!mPlaybackOngoing)
            {
               txtCtrl->Bind(wxEVT_TEXT_ENTER, [this](auto&) { OnOk(); });
               txtCtrl->Bind(wxEVT_TEXT, [this](wxCommandEvent& event) {
                  try
                  {
                     mClipSpeed = std::stod(event.GetString().ToStdString());
                  }
                  catch (const std::exception&)
                  {
                     // Something silly was entered; reset dialog
                     UpdateDialog();
                  }
               });
            }
            s.AddFixedText(Verbatim("%"));
         }
      }
   }

   ScopedHorizontalLay h { s, wxEXPAND, 0 };
   {
      ScopedInvisiblePanel panel { s, 10 };
      s.SetBorder(2);
      {
         ScopedHorizontalLay h { s, wxALIGN_RIGHT, 0 };
         s.AddSpace(270, 0, 0);
         s.AddButton(XXO("&Cancel"))->Bind(wxEVT_BUTTON, [this](auto&) {
            OnCancel();
         });
         auto okBtn = s.AddButton(XXO("&Ok"));
         okBtn->Bind(wxEVT_BUTTON, [this](auto&) { OnOk(); });
         okBtn->SetDefault();
      }
   }
}

void PitchAndSpeedDialog::OnOk()
{
   SetSemitoneShift();
   if (SetClipSpeedFromDialog())
      EndModal(wxID_OK);
}

void PitchAndSpeedDialog::OnCancel()
{
   mShift = mOldShift;
   SetSemitoneShift();
   EndModal(wxID_CANCEL);
}

bool PitchAndSpeedDialog::SetClipSpeedFromDialog()
{
   {
      ShuttleGui S(this, eIsGettingFromDialog);
      PopulateOrExchange(S);
   }

   if (mClipSpeed <= 0.0)
   {
      BasicUI::ShowErrorDialog(
         /* i18n-hint: Title of an error message shown, when invalid clip speed
            is set */
         wxWidgetsWindowPlacement { this }, XO("Invalid clip speed"),
         XO("Clip speed must be a positive value"), {});

      return false;
   }

   if (!TimeStretching::SetClipStretchRatio(
          mTrack, mTrackInterval, 100 / mClipSpeed))
   {
      BasicUI::ShowErrorDialog(
         wxWidgetsWindowPlacement { this }, XO("Invalid clip speed"),
         XO("There is not enough space to stretch the clip to the selected speed"),
         {});

      return false;
   }
   else if (const auto project = mProject.lock())
      WaveClipUIUtilities::SelectClip(*project, mTrackInterval);

   {
      ShuttleGui S(this, eIsSettingToDialog);
      PopulateOrExchange(S);
   }

   return true;
}

void PitchAndSpeedDialog::OnPitchShiftChange(bool semitonesChanged)
{
   // Rules:
   // 1. total shift is clipped to [minSemis, maxSemis]
   // 2. cents must be in the range [-100, 100]
   // 3. on semitone updates, keep semitone and cent sign consistent
   const auto newCentShift = mShift.semis * 100 + mShift.cents;
   static_assert(TimeAndPitchInterface::MaxCents % 100 == 0);
   static_assert(TimeAndPitchInterface::MinCents % 100 == 0);
   std::optional<PitchShift> correctedShift;
   if (newCentShift > TimeAndPitchInterface::MaxCents)
      correctedShift = PitchShift { TimeAndPitchInterface::MaxCents / 100, 0 };
   else if (newCentShift < TimeAndPitchInterface::MinCents)
      correctedShift = PitchShift { TimeAndPitchInterface::MinCents / 100, 0 };
   else if (mShift.cents == 100)
      correctedShift = PitchShift { mShift.semis + 1, 0 };
   else if (mShift.cents == -100)
      correctedShift = PitchShift { mShift.semis - 1, 0 };
   else if (semitonesChanged && mShift.cents > 0 && mShift.semis < 0)
      correctedShift = PitchShift { mShift.semis + 1, mShift.cents - 100 };
   else if (semitonesChanged && mShift.cents < 0 && mShift.semis > 0)
      correctedShift = PitchShift { mShift.semis - 1, mShift.cents + 100 };

   if (correctedShift.has_value())
      mShift = *correctedShift;
   SetSemitoneShift();
   if (correctedShift.has_value())
      UpdateDialog();
}

void PitchAndSpeedDialog::UpdateDialog()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);
}

void PitchAndSpeedDialog::SetSemitoneShift()
{
   const auto success =
      mTrackInterval.SetCentShift(mShift.semis * 100 + mShift.cents);
   assert(success);
}
