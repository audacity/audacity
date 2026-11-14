/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CompressionMeterPanel.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CompressionMeterPanel.h"
#include "AudioIO.h"
#include "CompressorInstance.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include <algorithm>
#include <limits>
#include <wx/dcclient.h>
#include <wx/graphics.h>

namespace {
using namespace DynamicRangeProcessorPanel;

constexpr auto timerId = 7000;
// An overestimate: 44.1kHz and 512 samples per buffer, in frames per second,
// would be 86.13.
constexpr auto audioFramePerSec = 200;
constexpr auto ticksPerSec = 1000 / compressorMeterUpdatePeriodMs;
// This will be set as max size for the lock-free queue.
constexpr auto audioFramesPerTick = audioFramePerSec / ticksPerSec;
} // namespace

BEGIN_EVENT_TABLE(CompressionMeterPanel, wxPanelWrapper)
EVT_PAINT(CompressionMeterPanel::OnPaint)
EVT_TIMER(timerId, CompressionMeterPanel::OnTimer)
END_EVENT_TABLE()

CompressionMeterPanel::CompressionMeterPanel(
    wxWindow* parent, int id, CompressorInstance& instance, float dbRange, std::function<void()> onClipped)
    : wxPanelWrapper{parent, id}
    , mMeterValuesQueue{std::make_unique<
                            DynamicRangeProcessorMeterValuesQueue>(audioFramesPerTick)}
    , mPlaybackStartStopSubscription{static_cast<
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
    })},
    mPlaybackPausedSubscription { AudioIO::Get()->Subscribe(
                                      [this](const AudioIOEvent& evt) {
        if (evt.type != AudioIOEvent::PAUSE) {
            return;
        }
        if (evt.on) {
            mTimer.Stop();
        } else {
            mTimer.Start(compressorMeterUpdatePeriodMs);
        }
    }) },
mOnClipped { onClipped },
mDbBottomEdgeValue { -dbRange },
mCompressionMeter { MeterValueProvider::Create(
                        MeterValueProvider::Direction::Downwards) },
mOutputMeter { MeterValueProvider::Create(
                   MeterValueProvider::Direction::Upwards) }
{
    if (instance.GetSampleRate().has_value()) {
        // Playback is ongoing, and so the `InitializeProcessingSettings` event
        // was already fired.
        Reset();
    }
    instance.SetMeterValuesQueue(mMeterValuesQueue);
    mTimer.SetOwner(this, timerId);
    SetDoubleBuffered(true);
}

void CompressionMeterPanel::SetDbRange(float dbRange)
{
    mDbBottomEdgeValue = -dbRange;
    Refresh(true);
}

void CompressionMeterPanel::Reset()
{
    mCompressionMeter
        =MeterValueProvider::Create(MeterValueProvider::Direction::Downwards);
    mOutputMeter
        =MeterValueProvider::Create(MeterValueProvider::Direction::Upwards);
    mStopWhenZero = false;
    mClipped = false;
    mTimer.Start(compressorMeterUpdatePeriodMs);
}

void CompressionMeterPanel::ResetClipped()
{
    mClipped = false;
}

void CompressionMeterPanel::OnPaint(wxPaintEvent& evt)
{
    using namespace DynamicRangeProcessorPanel;

    wxPaintDC dc(this);

    const auto rect = DynamicRangeProcessorPanel::GetPanelRect(*this);
    const auto gc = MakeGraphicsContext(dc);
    const auto left = rect.GetLeft();
    const auto top = rect.GetTop();
    const auto width = rect.GetWidth();
    const auto height = rect.GetHeight();

    gc->SetPen(*wxTRANSPARENT_PEN);
    gc->SetBrush(GetGraphBackgroundBrush(*gc, height));
    gc->DrawRectangle(left, top, width, height);

    auto leftRect = rect;
    leftRect.SetWidth(rect.GetWidth() / 2 - 2);
    leftRect.Offset(1, 0);
    PaintMeter(dc, actualCompressionColor, leftRect, *mCompressionMeter);

    auto rightRect = leftRect;
    rightRect.Offset(leftRect.GetWidth(), 0);
    PaintMeter(dc, outputColor, rightRect, *mOutputMeter);

    gc->SetPen(lineColor);
    gc->SetBrush(wxNullBrush);
    gc->DrawRectangle(left, top, width, height);
}

void CompressionMeterPanel::PaintMeter(
    wxPaintDC& dc, const wxColor& color, const wxRect& rect,
    const MeterValueProvider& provider)
{
    const auto dB = provider.GetCurrentMax();
    const auto maxDb = provider.GetGlobalMax();
    const auto fiveSecMaxDb = provider.GetFiveSecMax();
    const auto downwards
        =provider.GetDirection() == MeterValueProvider::Direction::Downwards;

    const auto gc = MakeGraphicsContext(dc);

    const double left = rect.GetLeft();
    const double top = rect.GetTop();
    const double width = rect.GetWidth();
    const double height = rect.GetHeight();

    constexpr auto lineWidth = 6.;

    const double dbFrac = std::clamp<double>(dB / mDbBottomEdgeValue, 0., 1.);
    // So that the top of the cap is aligned with the dB value.
    const auto yAdjust = lineWidth / 2 * (downwards ? -1 : 1);
    const double dbY = height * dbFrac + yAdjust;
    const double maxDbY = maxDb / mDbBottomEdgeValue * height + yAdjust;
    const double fiveSecMaxDbY
        =fiveSecMaxDb / mDbBottomEdgeValue * height + yAdjust;

    const auto levelTop = downwards ? top : dbY;
    const auto levelHeight = downwards ? dbY : height - dbY;
    const auto maxLevelTop = downwards ? levelHeight - top : maxDbY;
    const auto maxLevelHeight = std::abs(maxDbY - dbY);

    const auto rectLeft = left + 3;
    const auto rectWidth = width - 4;
    const auto opaqueColor = wxColor { color.GetRGB() };
    gc->SetBrush(GetColorMix(opaqueColor, wxTransparentColor, 0.8));
    gc->DrawRectangle(rectLeft, levelTop, rectWidth, levelHeight);
    gc->SetBrush(GetColorMix(opaqueColor, wxTransparentColor, 0.6));
    gc->DrawRectangle(rectLeft, maxLevelTop, rectWidth, maxLevelHeight);
    gc->SetBrush(opaqueColor);
    gc->DrawRectangle(rectLeft, dbY - 2, rectWidth, lineWidth);
    gc->DrawRectangle(rectLeft, maxDbY - 2, rectWidth, lineWidth);
    gc->SetPen({ opaqueColor, static_cast<int>(lineWidth / 2) });
    gc->StrokeLine(
        left + rectWidth / 2, fiveSecMaxDbY, left + rectWidth, fiveSecMaxDbY);
}

void CompressionMeterPanel::OnTimer(wxTimerEvent& evt)
{
    // Take the max of all values newly pushed by the audio frames - make sure we
    // don't miss a peak.
    MeterValues values;
    auto lowestCompressionGain = 0.f;
    auto highestOutputGain = std::numeric_limits<float>::lowest();
    while (mMeterValuesQueue->Get(values))
    {
        lowestCompressionGain
            =std::min(values.compressionGainDb, lowestCompressionGain);
        highestOutputGain = std::max(values.outputDb, highestOutputGain);
    }

    const auto updateFiveSecondMax = !mStopWhenZero;
    mCompressionMeter->Update(lowestCompressionGain, updateFiveSecondMax);
    mOutputMeter->Update(highestOutputGain, updateFiveSecondMax);
    const auto clipped = mOutputMeter->GetCurrentMax() > 0;
    if (clipped && !mClipped) {
        mOnClipped();
        mClipped = true;
    }

    Refresh(false);

    if (
        mCompressionMeter->IsInvisible() && mOutputMeter->IsInvisible()
        && mStopWhenZero) {
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
