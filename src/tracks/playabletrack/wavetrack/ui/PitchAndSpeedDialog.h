/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file PitchAndSpeedDialog.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include "WaveTrack.h"
#include "wxPanelWrapper.h"

class ShuttleGui;

enum class PitchAndSpeedDialogFocus
{
   Pitch,
   Speed
};

class PitchAndSpeedDialog final : public wxDialogWrapper
{
public:
   PitchAndSpeedDialog(
      bool playbackOngoing, WaveTrack& track, WaveTrack::Interval& interval,
      wxWindow* parent,
      const std::optional<PitchAndSpeedDialogFocus>& focus = {});

   ~PitchAndSpeedDialog() override;

private:
   void PopulateOrExchange(
      ShuttleGui& s, const std::optional<PitchAndSpeedDialogFocus>& focus = {});

   void OnOk();

   void OnCancel();

   bool SetClipSpeedFromDialog();

   void OnPitchShiftChange(bool semitonesChanged);
   void SetSemitoneShift();
   void UpdateDialog();

   const bool mPlaybackOngoing;
   WaveTrack& mTrack;
   WaveTrack::Interval& mTrackInterval;
   double mClipSpeed;
   const double mOldClipSpeed;
   struct PitchShift
   {
      int semis = 0;
      int cents = 0;
   };
   PitchShift mShift;
   PitchShift mOldShift;
};
