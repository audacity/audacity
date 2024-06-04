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
const wxColor compressionColor { 255, 219, 0 };
const wxColor compressionMaxColor = compressionColor.ChangeLightness(70);
const wxColor startColor = GetColorMix(backgroundColor, compressionColor, w);
} // namespace

BEGIN_EVENT_TABLE(CompressionMeterPanel, wxPanelWrapper)
EVT_PAINT(CompressionMeterPanel::OnPaint)
EVT_TIMER(timerId, CompressionMeterPanel::OnTimer)
END_EVENT_TABLE()

CompressionMeterPanel::CompressionMeterPanel(
   wxWindow* parent, CompressorInstance& instance)
    : wxPanelWrapper { parent, wxID_ANY }
    , mCompressorInstance { instance }
    , mCompressionValueQueue { std::make_unique<LockFreeQueue<float>>(
         audioFramesPerTick) }
    , mPlaybackStartStopSubscription {
       static_cast<InitializeProcessingSettingsPublisher&>(instance).Subscribe(
          [&](const std::optional<InitializeProcessingSettings>& evt) {
             if (evt)
                Reset();
             else
                mStopWhenZero = true;
          })
    }
{
   if (instance.GetSampleRate().has_value())
      // Playback is ongoing, and so the `InitializeProcessingSettings` event
      // was already fired.
      Reset();
   mCompressorInstance.SetCompressionValueQueue(mCompressionValueQueue);
   mTimer.SetOwner(this, timerId);
   SetDoubleBuffered(true);
}

void CompressionMeterPanel::Reset()
{
   mCompressionYMax = 0;
   mCompressionRingBuffer.fill(0);
   mStopWhenZero = false;
   mTimer.Start(timerPeriodMs);
}

void CompressionMeterPanel::OnPaint(wxPaintEvent& evt)
{
   using namespace DynamicRangeProcessorPanel;

   wxPaintDC dc(this);

   const auto gc = MakeGraphicsContext(dc);

   const auto width = GetSize().GetWidth();
   const auto height = GetSize().GetHeight();
   const auto frac =
      std::clamp(-mSmoothedCompressionDb / compressorMeterRangeDb, 0., 1.);
   const auto compressionY = height * frac;

   mCompressionYMax = std::max(mCompressionYMax, compressionY);
   const auto fracMax = mCompressionYMax / height;

   gc->SetPen(wxNullPen);
   gc->SetBrush(gc->CreateLinearGradientBrush(
      0, 0, 0, height, startColor, compressionColor));
   gc->DrawRectangle(0, 0, width, compressionY);
   gc->SetBrush(backgroundColor);
   gc->DrawRectangle(0, compressionY, width, height - compressionY);

   const auto compressionLineColor =
      GetColorMix(compressionMaxColor, startColor, frac);
   gc->SetPen(wxPen { compressionLineColor, 2 });
   gc->StrokeLine(0, compressionY, width, compressionY);
   const auto maxCompressionLineColor =
      GetColorMix(compressionMaxColor, startColor, fracMax);
   gc->SetPen(wxPen { maxCompressionLineColor, 2 });
   gc->StrokeLine(0, mCompressionYMax, width, mCompressionYMax);

   dc.SetFont(
      { 12, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL });
   dc.SetTextForeground({ 128, 128, 128 });
   const auto text = XO("compression").Translation();
   const auto t = dc.GetTextExtent(text);
   const auto x = (width + t.GetHeight()) / 2;
   const auto y = (height - t.GetWidth()) / 2;
   dc.DrawRotatedText(text, x, y, 270);

   gc->SetPen(lineColor);
   gc->SetBrush(wxNullBrush);
   gc->DrawRectangle(0, 0, width, height);
}

void CompressionMeterPanel::OnTimer(wxTimerEvent& evt)
{
   const auto lastCompression =
      mCompressionRingBuffer[mCompressionRingBufferIndex];

   // Take the max of all values newly pushed by the audio frames - make sure we
   // don't miss a peak.
   auto compression = 0.f;
   auto maxCompression = 0.f;
   while (mCompressionValueQueue->Get(compression))
      maxCompression = std::min(compression, maxCompression);

   mCompressionRingBuffer[mCompressionRingBufferIndex] = maxCompression;
   mCompressionRingBufferIndex =
      (mCompressionRingBufferIndex + 1) % ringBufferLength;

   if (lastCompression < mSmoothedCompressionDb)
      mSmoothedCompressionDb = lastCompression;
   else
      mSmoothedCompressionDb =
         std::min(0., mSmoothedCompressionDb + decayPerTickDb);

   Refresh(false);

   if (mSmoothedCompressionDb == 0 && mStopWhenZero)
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
