/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DynamicRangeProcessorTypes.h"
#include "LockFreeQueue.h"
#include "Observer.h"
#include "wxPanelWrapper.h"
#include <array>
#include <wx/timer.h>

class CompressorInstance;
class wxPaintDC;

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
   void PaintRectangle(
      wxPaintDC& dc, const wxRect& rect, double value, double& yMax,
      const TranslatableString& label);

   void OnPaint(wxPaintEvent& evt);
   void OnTimer(wxTimerEvent& evt);

   bool AcceptsFocus() const override;
   // So that wxPanel is not included in Tab traversal - see wxWidgets bug 15581
   bool AcceptsFocusFromKeyboard() const override;

   const std::shared_ptr<DynamicRangeProcessorMeterValuesQueue>
      mMeterValuesQueue;
   const Observer::Subscription mPlaybackStartStopSubscription;
   wxTimer mTimer;
   MeterValues mSmoothedValues;
   bool mStopWhenZero = false;
   double mCompressionYMax = 0;
   double mOutputYMax = 0;
   std::array<double, ringBufferLength> mCompressionRingBuffer;
   size_t mCompressionRingBufferIndex = 0;
};
