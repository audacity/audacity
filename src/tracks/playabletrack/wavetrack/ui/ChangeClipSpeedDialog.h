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
      WaveTrack& track, WaveTrack::Interval& interval,
      wxWindow* parent = nullptr);

   ~ChangeClipSpeedDialog() override;

private:
   void PopulateOrExchange(ShuttleGui& s);

   void OnOk();

   bool SetClipSpeedFromDialog();

   WaveTrack& mTrack;
   WaveTrack::Interval& mTrackInterval;
   double mClipSpeed;
   double mOldClipSpeed;
};
