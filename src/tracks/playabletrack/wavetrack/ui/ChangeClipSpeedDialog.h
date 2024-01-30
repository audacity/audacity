/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ChangeClipSpeedDialog.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include "wxPanelWrapper.h"
#include "WaveTrack.h"

class ShuttleGui;

class ChangeClipSpeedDialog final : public wxDialogWrapper
{
public:
   ChangeClipSpeedDialog(
      bool playbackOngoing, WaveTrack& track, WaveTrack::Interval& interval,
      wxWindow* parent = nullptr);

   ~ChangeClipSpeedDialog() override;

private:
   void PopulateOrExchange(ShuttleGui& s);

   void OnOk();

   void OnCancel();

   bool SetClipSpeedFromDialog();

   void OnPitchShiftChange();
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
