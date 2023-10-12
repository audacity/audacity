/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ChangeClipSpeedDialog.cpp

 Dmitry Vedenko

 **********************************************************************/
#include "ChangeClipSpeedDialog.h"

#include <wx/button.h>
#include <wx/textctrl.h>

#include "ShuttleGui.h"
#include "wxWidgetsWindowPlacement.h"

ChangeClipSpeedDialog::ChangeClipSpeedDialog(
   WaveTrack& waveTrack, WaveTrack::Interval& interval, wxWindow* parent)
    : wxDialogWrapper(
         parent, wxID_ANY, XO("Change Speed"), wxDefaultPosition, { 480, 250 },
         wxDEFAULT_DIALOG_STYLE)
    , mTrack { waveTrack }
    , mTrackInterval { interval }
    , mClipSpeed { 100.0 / interval.GetStretchRatio() }
    , mOldClipSpeed { mClipSpeed }
{
   ShuttleGui s(this, eIsCreating);

   s.StartVerticalLay();
   {
      PopulateOrExchange(s);
   }
   s.EndVerticalLay();

   // TODO: Tolerance?
   // Stretch ratio of 
   assert(mOldClipSpeed > 0.0);

   Layout();
   Fit();
   Centre();

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

ChangeClipSpeedDialog::~ChangeClipSpeedDialog() = default;

void ChangeClipSpeedDialog::PopulateOrExchange(ShuttleGui& s)
{
   s.StartInvisiblePanel(15);
   s.StartVerticalLay(wxEXPAND, 1);
   {
      s.SetBorder(1);
      s.StartHorizontalLay(wxEXPAND);
      {
         s.AddPrompt(XO("Clip speed"));
         s.AddSpace(0, 0, 1);
         s.Name(XO("Clip speed"))
            .NameSuffix(Verbatim("%"))
            .TieNumericTextBox({}, mClipSpeed, 14, true)
            ->Bind(wxEVT_TEXT_ENTER, [this](auto&) { OnOk(); });

         s.AddFixedText(Verbatim("%"));
      }
      s.EndMultiColumn();
   }
   s.EndHorizontalLay();
   s.EndInvisiblePanel();

   s.StartHorizontalLay(wxEXPAND, 0);
   {
      s.StartInvisiblePanel(14);
      {
         s.SetBorder(2);
         s.StartHorizontalLay(wxEXPAND, 0);
         {
            s.AddSpace(0, 0, 1);

            s.AddButton(XXO("&Cancel"))
               ->Bind(wxEVT_BUTTON, [this](auto&) { EndModal(wxID_CANCEL); });

            s.AddSpace(4, 0, 0);

            auto okBtn = s.AddButton(XXO("&Ok"));
            okBtn->Bind(wxEVT_BUTTON, [this](auto&) { OnOk(); });
            okBtn->SetDefault();
         }
         s.EndHorizontalLay();
      }
      s.EndInvisiblePanel();
   }
   s.EndHorizontalLay();
}

void ChangeClipSpeedDialog::OnOk()
{
   if (SetClipSpeedFromDialog())
      EndModal(wxID_OK);
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
         /* i18n-hint: Title of an error message shown, when invalid clip speed is set */
         wxWidgetsWindowPlacement { this }, XO("Invalid clip speed"),
         XO("Clip speed must be a positive value"), {});

      return false;
   }

   const auto nextClip = mTrack.GetNextInterval(mTrackInterval, PlaybackDirection::forward);
   const auto maxEndTime = nextClip != nullptr ?
                              nextClip->Start() :
                              std::numeric_limits<double>::infinity();

   const auto start = mTrackInterval.Start();
   const auto end = mTrackInterval.End();

   const auto rate = mTrack.GetRate();
   const auto newDuration = std::floor((end - start) * mOldClipSpeed / mClipSpeed * rate + 0.5) / rate;

   const auto expectedEndTime =
      start + std::max(newDuration, 1.0 / rate);

   if (expectedEndTime >= maxEndTime)
   {
      BasicUI::ShowErrorDialog(
         wxWidgetsWindowPlacement { this }, XO("Invalid clip speed"),
         XO("There is not enough space to stretch the clip to the selected speed"), {});

      return false;
   }

   mTrackInterval.Stretch(newDuration);
   mOldClipSpeed = mClipSpeed = 100 / mTrackInterval.GetStretchRatio();

   {
      ShuttleGui S(this, eIsSettingToDialog);
      PopulateOrExchange(S);
   }

   return true;
}
