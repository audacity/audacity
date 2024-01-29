/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ChangeClipSpeedDialog.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "ChangeClipSpeedDialog.h"

#include <wx/button.h>
#include <wx/layout.h>
#include <wx/spinctrl.h>
#include <wx/textctrl.h>

#include "ShuttleGui.h"
#include "SpinControl.h"
#include "wxWidgetsWindowPlacement.h"

namespace
{
template <typename ReturnType, typename... Args> class ScopedSizer
{
public:
   ScopedSizer(
      ShuttleGui& s, ReturnType (ShuttleGui::*startFunc)(Args...),
      void (ShuttleGui::*endFunc)(), Args&&... args)
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
            std::move(PositionFlags), std::move(iProp))
   {
   }
};

class ScopedInvisiblePanel : public ScopedSizer<wxPanel*, int>
{
public:
   ScopedInvisiblePanel(ShuttleGui& s, int border = 0)
       : ScopedSizer<wxPanel*, int>(
            s, &ShuttleGui::StartInvisiblePanel, &ShuttleGui::EndInvisiblePanel,
            std::move(border))
   {
   }
};

class ScopedVerticalLay : public ScopedSizer<void, int>
{
public:
   ScopedVerticalLay(ShuttleGui& s, int iProp = 1)
       : ScopedSizer<void, int>(
            s, &ShuttleGui::StartVerticalLay, &ShuttleGui::EndVerticalLay,
            std::move(iProp))
   {
   }
};
} // namespace

ChangeClipSpeedDialog::ChangeClipSpeedDialog(
   bool playbackOngoing, WaveTrack& waveTrack, WaveTrack::Interval& interval,
   wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Pitch and Speed"), wxDefaultPosition,
         { 480, 250 }, wxDEFAULT_DIALOG_STYLE)
    , mPlaybackOngoing { playbackOngoing }
    , mTrack { waveTrack }
    , mTrackInterval { interval }
    , mClipSpeed { 100.0 / interval.GetStretchRatio() }
    , mOldClipSpeed { mClipSpeed }
{
   const auto shift = mTrackInterval.GetSemitoneShift();
   mShift.semis = std::floor(shift);
   mShift.cents = std::round((shift - mShift.semis) * 100);
   mOldShift = mShift;

   ShuttleGui s(this, eIsCreating);

   {
      ScopedVerticalLay v { s };
      PopulateOrExchange(s);
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

ChangeClipSpeedDialog::~ChangeClipSpeedDialog() = default;

void ChangeClipSpeedDialog::PopulateOrExchange(ShuttleGui& s)
{
   {
      ScopedInvisiblePanel panel { s, 15 };
      s.SetBorder(0);
      {
         ScopedHorizontalLay h { s, wxLeft };
         s.AddPrompt(XO("Clip Pitch"));
      }
      {
         ScopedHorizontalLay h { s, wxLeft };
         constexpr auto minSemis = -24;
         constexpr auto maxSemis = 60;
         s.SetBorder(2);
         s.TieSpinCtrl(Verbatim(""), mShift.semis, maxSemis, minSemis)
            ->Bind(wxEVT_TEXT, [&](wxCommandEvent& event) {
               mShift.semis = event.GetInt();
               if (mShift.semis == maxSemis && mShift.cents != 0)
               {
                  mShift.cents = 0;
                  SetSemitoneShift();
                  UpdateDialog();
               }
               else
                  SetSemitoneShift();
            });
         s.AddSpace(1, 0);
         s.AddFixedText(XO("semitones,"));
         s.TieSpinCtrl(Verbatim(""), mShift.cents, 100, -1)
            ->Bind(wxEVT_TEXT, [&](wxCommandEvent& event) {
               mShift.cents = event.GetInt();
               std::optional<PitchShift> correctedShift;
               if (mShift.semis == maxSemis && mShift.cents != -1)
                  // If we've reached the higher limit, unless the user wants to
                  // go down, the cents stay at 0.
                  correctedShift = PitchShift { maxSemis, 0 };
               else if (mShift.cents == -1)
               {
                  if (mShift.semis > minSemis)
                     // Going down is allowed.
                     correctedShift = PitchShift { mShift.semis - 1, 99 };
                  else
                     // We're already at the min transposition.
                     correctedShift = PitchShift { minSemis, 0 };
               }
               else if (mShift.cents == 100)
                  // Going up is allowed.
                  correctedShift = PitchShift { mShift.semis + 1, 0 };
               if (correctedShift.has_value())
                  mShift = *correctedShift;
               SetSemitoneShift();
               if (correctedShift.has_value())
                  UpdateDialog();
            });
         s.AddFixedText(XO("cents"));
      }

      s.AddSpace(0, 12);
      s.SetBorder(0);

      {
         ScopedHorizontalLay h { s, wxLeft };
         s.AddPrompt(XO("Clip Speed"));
      }
      {
         ScopedHorizontalLay h { s, wxLeft };
         auto txtCtrl = s.NameSuffix(Verbatim("%"))
                           .TieNumericTextBox({}, mClipSpeed, 14, true);
         txtCtrl->Enable(!mPlaybackOngoing);
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
               }
            });
         }
         s.AddFixedText(Verbatim("%"));
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

void ChangeClipSpeedDialog::OnOk()
{
   SetSemitoneShift();
   if (SetClipSpeedFromDialog())
      EndModal(wxID_OK);
}

void ChangeClipSpeedDialog::OnCancel()
{
   mShift = mOldShift;
   SetSemitoneShift();
   EndModal(wxID_CANCEL);
}

bool ChangeClipSpeedDialog::SetClipSpeedFromDialog()
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

   const auto nextClip =
      mTrack.GetNextInterval(mTrackInterval, PlaybackDirection::forward);
   const auto maxEndTime = nextClip != nullptr ?
                              nextClip->Start() :
                              std::numeric_limits<double>::infinity();

   const auto start = mTrackInterval.Start();
   const auto end = mTrackInterval.End();

   const auto expectedEndTime =
      start + (end - start) * mOldClipSpeed / mClipSpeed;

   if (expectedEndTime >= maxEndTime)
   {
      BasicUI::ShowErrorDialog(
         wxWidgetsWindowPlacement { this }, XO("Invalid clip speed"),
         XO("There is not enough space to stretch the clip to the selected speed"),
         {});

      return false;
   }

   mTrackInterval.StretchRightTo(expectedEndTime);

   {
      ShuttleGui S(this, eIsSettingToDialog);
      PopulateOrExchange(S);
   }

   return true;
}

void ChangeClipSpeedDialog::UpdateDialog()
{
   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);
}

void ChangeClipSpeedDialog::SetSemitoneShift()
{
   mTrackInterval.SetSemitoneShift(mShift.semis + mShift.cents / 100.0);
}
