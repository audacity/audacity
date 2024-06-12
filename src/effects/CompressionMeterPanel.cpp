/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CompressionMeterPanel.h"
#include "CompressorInstance.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include <algorithm>
#include <limits>
#include <wx/dcclient.h>
#include <wx/graphics.h>

namespace
{
using namespace DynamicRangeProcessorPanel;

constexpr auto timerId = 7000;
constexpr auto decayPerSecondDb = 10.f;
constexpr auto decayPerTickDb =
   decayPerSecondDb * CompressionMeterPanel::timerPeriodMs / 1000.f;
// An overestimate: 44.1kHz and 512 samples per buffer, in frames per second,
// would be 86.13.
constexpr auto audioFramePerSec = 200;
constexpr auto ticksPerSec = 1000 / CompressionMeterPanel::timerPeriodMs;
// This will be set as max size for the lock-free queue.
constexpr auto audioFramesPerTick = audioFramePerSec / ticksPerSec;

constexpr auto w = 0.4;
const wxColor fullLevelColor { 255, 219, 0 };
const wxColor levelMinColor = GetColorMix(backgroundColor, fullLevelColor, w);
const wxColor levelMaxColor = fullLevelColor.ChangeLightness(70);
} // namespace

BEGIN_EVENT_TABLE(CompressionMeterPanel, wxPanelWrapper)
EVT_PAINT(CompressionMeterPanel::OnPaint)
EVT_TIMER(timerId, CompressionMeterPanel::OnTimer)
END_EVENT_TABLE()

CompressionMeterPanel::CompressionMeterPanel(
   wxWindow* parent, int id, CompressorInstance& instance, float dbRange)
    : wxPanelWrapper { parent, id }
    , mMeterValuesQueue { std::make_unique<
         DynamicRangeProcessorMeterValuesQueue>(audioFramesPerTick) }
    , mPlaybackStartStopSubscription { static_cast<
                                          InitializeProcessingSettingsPublisher&>(
                                          instance)
                                          .Subscribe(
                                             [&](const std::optional<
                                                 InitializeProcessingSettings>&
                                                    evt) {
                                                if (evt)
                                                   Reset();
                                                else
                                                   mStopWhenZero = true;
                                             }) }
    , mOutputColBottomValue { -dbRange }
{
   if (instance.GetSampleRate().has_value())
      // Playback is ongoing, and so the `InitializeProcessingSettings` event
      // was already fired.
      Reset();
   instance.SetMeterValuesQueue(mMeterValuesQueue);
   mTimer.SetOwner(this, timerId);
   SetDoubleBuffered(true);
}

void CompressionMeterPanel::SetDbRange(float dbRange)
{
   mOutputColBottomValue = -dbRange;
   Refresh(true);
}

void CompressionMeterPanel::Reset()
{
   mCompressionColMin = 0;
   mOutputColMax = mOutputColBottomValue;
   mRingBuffer.fill({});
   mStopWhenZero = false;
   mTimer.Start(timerPeriodMs);
}

void CompressionMeterPanel::OnPaint(wxPaintEvent& evt)
{
   using namespace DynamicRangeProcessorPanel;

   wxPaintDC dc(this);

   auto left = GetClientRect();
   left.SetWidth(left.GetWidth() / 2);
   PaintRectangle(
      dc, left, mSmoothedValues.compressionGainDb, mCompressionColMin, true);
   PaintLabel(dc, left, XO("compression"));

   auto right = left;
   right.Offset(left.GetWidth(), 0);
   PaintRectangle(dc, right, mSmoothedValues.outputDb, mOutputColMax, false);
   PaintLabel(dc, right, XO("output"));

   const auto gc = MakeGraphicsContext(dc);
   gc->SetPen(lineColor);
   gc->SetBrush(wxNullBrush);
   gc->DrawRectangle(0, 0, GetSize().GetWidth(), GetSize().GetHeight());
}

void CompressionMeterPanel::PaintRectangle(
   wxPaintDC& dc, const wxRect& rect, double dB, double maxDb, bool upwards)
{
   const auto gc = MakeGraphicsContext(dc);

   const double left = rect.GetLeft();
   const double top = rect.GetTop();
   const double width = rect.GetWidth();
   const double height = rect.GetHeight();

   const double dbFrac = std::clamp<double>(dB / mOutputColBottomValue, 0., 1.);
   const double dbY = height * dbFrac;
   const double maxDbY = maxDb / mOutputColBottomValue * height;
   const double maxDbFrac = maxDbY / height;

   const auto levelTop = upwards ? top : dbY;
   const auto backgroundTop = upwards ? dbY : top;
   const auto levelHeight = upwards ? dbY : height - dbY;
   const auto levelMinColorY = upwards ? 0 : height;
   const auto fullLevelColorY = height - levelMinColorY;
   const auto& bottomColor = upwards ? levelMaxColor : levelMinColor;
   const auto& topColor = upwards ? levelMinColor : levelMaxColor;

   gc->SetPen(wxNullPen);
   gc->SetBrush(gc->CreateLinearGradientBrush(
      0, levelMinColorY, 0, fullLevelColorY, levelMinColor, fullLevelColor));
   gc->DrawRectangle(left, levelTop, width, levelHeight);
   gc->SetBrush(backgroundColor);
   gc->DrawRectangle(left, backgroundTop, width, height - levelHeight);

   const auto levelLineColor = GetColorMix(bottomColor, topColor, dbFrac);
   gc->SetPen(wxPen { levelLineColor, 1 });
   gc->StrokeLine(left, dbY, left + width, dbY);
   const auto levelMaxLineColor = GetColorMix(bottomColor, topColor, maxDbFrac);
   gc->SetPen(wxPen { levelMaxLineColor, 1 });
   gc->StrokeLine(left, maxDbY, left + width, maxDbY);
}

void CompressionMeterPanel::PaintLabel(
   wxPaintDC& dc, const wxRect& rect, const TranslatableString& label)
{
   const auto left = rect.GetLeft();
   const auto width = rect.GetWidth();
   const auto height = rect.GetHeight();
   dc.SetFont(
      { 10, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL });
   dc.SetTextForeground({ 128, 128, 128 });
   const auto text = label.Translation();
   const auto t = dc.GetTextExtent(text);
   const auto x = left + (width + t.GetHeight()) / 2;
   const auto y = height - t.GetWidth() - 5;
   dc.DrawRotatedText(text, x, y, 270);
}

void CompressionMeterPanel::OnTimer(wxTimerEvent& evt)
{
   const auto lastValues = mRingBuffer[mRingBufferIndex];

   // Take the max of all values newly pushed by the audio frames - make sure we
   // don't miss a peak.
   MeterValues values;
   auto lowestCompressionGain = 0.f;
   auto highestOutputGain = std::numeric_limits<float>::lowest();
   while (mMeterValuesQueue->Get(values))
   {
      lowestCompressionGain =
         std::min(values.compressionGainDb, lowestCompressionGain);
      highestOutputGain = std::max(values.outputDb, highestOutputGain);
   }

   mRingBuffer[mRingBufferIndex] = { lowestCompressionGain, highestOutputGain };
   mRingBufferIndex = (mRingBufferIndex + 1) % ringBufferLength;

   if (lastValues.compressionGainDb < mSmoothedValues.compressionGainDb)
   {
      mSmoothedValues.compressionGainDb = lastValues.compressionGainDb;
      mCompressionColMin =
         std::min<double>(mCompressionColMin, lastValues.compressionGainDb);
   }
   else
      mSmoothedValues.compressionGainDb =
         mSmoothedValues.compressionGainDb + decayPerTickDb;

   if (lastValues.outputDb > mSmoothedValues.outputDb)
   {
      mSmoothedValues.outputDb = lastValues.outputDb;
      mOutputColMax = std::max<double>(mOutputColMax, lastValues.outputDb);
   }
   else
      mSmoothedValues.outputDb = mSmoothedValues.outputDb - decayPerTickDb;

   Refresh(false);

   if (
      mSmoothedValues.compressionGainDb >= 0 &&
      mSmoothedValues.outputDb <=
         mOutputColBottomValue -
            30 // It can be that the user augments the height after playback
               // stopped, so we must should continue decaying for a while even
               // after having reached the current bottom.
      && mStopWhenZero)
   {
      // Decay is complete. Until playback starts again, no need for
      // timer-triggered updates.
      mTimer.Stop();
      mStopWhenZero = false;
   }
}

bool CompressionMeterPanel::AcceptsFocus() const
{
   return false;
}

bool CompressionMeterPanel::AcceptsFocusFromKeyboard() const
{
   return false;
}
