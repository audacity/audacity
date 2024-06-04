/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "LockFreeQueue.h"
#include "Observer.h"
#include "wxPanelWrapper.h"
#include <array>
#include <wx/timer.h>

class CompressorInstance;

class CompressionMeterPanel final : public wxPanelWrapper
{
public:
   CompressionMeterPanel(wxWindow* parent, CompressorInstance& instance);

   static constexpr auto timerPeriodMs = 1000 / 30;

   DECLARE_EVENT_TABLE();

private:
   //! The display tends to be earlier than the audio playback. We delay the
   //! former by (approximately) this amount, for a tighter audiovisual
   //! experience.
   static constexpr auto displayDelayMs = 100;
   static constexpr auto ringBufferLength = displayDelayMs / timerPeriodMs;

   void Reset();

   void OnPaint(wxPaintEvent& evt);
   void OnTimer(wxTimerEvent& evt);

   bool AcceptsFocus() const override;
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override;

   CompressorInstance& mCompressorInstance;
   const std::shared_ptr<LockFreeQueue<float>> mCompressionValueQueue;
   const Observer::Subscription mPlaybackStartStopSubscription;
   wxTimer mTimer;
   double mSmoothedCompressionDb = 0;
   bool mStopWhenZero = false;
   double mCompressionYMax = 0;
   std::array<double, ringBufferLength> mCompressionRingBuffer;
   size_t mCompressionRingBufferIndex = 0;
};
