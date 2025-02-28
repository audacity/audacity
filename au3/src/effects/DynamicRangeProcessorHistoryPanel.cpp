/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorHistoryPanel.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorHistoryPanel.h"
#include "AColor.h"
#include "AllThemeResources.h"
#include "AudioIO.h"
#include "CompressorInstance.h"
#include "DynamicRangeProcessorHistory.h"
#include "DynamicRangeProcessorPanelCommon.h"
#include "Theme.h"
#include "widgets/LinearDBFormat.h"
#include "widgets/LinearUpdater.h"
#include "widgets/Ruler.h"
#include <cassert>
#include <cmath>
#include <numeric>
#include <wx/dcclient.h>
#include <wx/graphics.h>
#include <wx/platinfo.h>

namespace {
constexpr auto timerId = 7000;
// Of course we aren't really targetting 200fps, but when specifying 50fps, we
// rather get 30fps, with outliers at 20. Measurements (Windows) showed that,
// when specifying 200, we get around 60fps on average, with outlier around 40.
constexpr auto timerPeriodMs = 1000 / 200;

bool MayUsePenGradients()
{
    // MacOS doesn't cope well with pen gradients. (Freezes in debug and is
    // transperent in release.)
    return wxPlatformInfo::Get().GetOperatingSystemId() & wxOS_WINDOWS;
}

int GetActualCompressionLineWidth()
{
    using namespace DynamicRangeProcessorPanel;
    return targetCompressionLineWidth + (MayUsePenGradients() ? 4 : 2);
}
} // namespace

BEGIN_EVENT_TABLE(DynamicRangeProcessorHistoryPanel, wxPanelWrapper)
EVT_PAINT(DynamicRangeProcessorHistoryPanel::OnPaint)
EVT_SIZE(DynamicRangeProcessorHistoryPanel::OnSize)
EVT_TIMER(timerId, DynamicRangeProcessorHistoryPanel::OnTimer)
END_EVENT_TABLE()

DynamicRangeProcessorHistoryPanel::DynamicRangeProcessorHistoryPanel(
    wxWindow* parent, wxWindowID winid, CompressorInstance& instance, std::function<void(float)> onDbRangeChanged)
    : wxPanelWrapper{parent, winid}
    , mCompressorInstance{instance}
    , mOnDbRangeChanged{std::move(onDbRangeChanged)}
    , mInitializeProcessingSettingsSubscription{static_cast<
                                                    InitializeProcessingSettingsPublisher&>(
                                                    instance)
                                                .Subscribe(
                                                    [&](const std::optional<
                                                            InitializeProcessingSettings>&
                                                        evt) {
        if (evt)
            InitializeForPlayback(
                instance,
                evt->sampleRate);
        else
            // Stop the
            // timer-based
            // update but keep
            // the history
            // until playback
            // is resumed.
            mTimer.Stop();
    })},
    mRealtimeResumeSubscription { static_cast<RealtimeResumePublisher&>(
                                      instance)
                                  .Subscribe([this](auto) {
        if (mHistory) {
            mHistory->BeginNewSegment();
        }
    }) },
mPlaybackEventSubscription { AudioIO::Get()->Subscribe(
                                 [this](const AudioIOEvent& evt) {
        if (evt.type != AudioIOEvent::PAUSE) {
            return;
        }
        if (evt.on) {
            mTimer.Stop();
            mClock.Pause();
        } else {
            mClock.Resume();
            mTimer.Start(timerPeriodMs);
        }
    }) }
{
    if (const auto& sampleRate = instance.GetSampleRate();
        sampleRate.has_value()) {
        // Playback is ongoing, and so the `InitializeProcessingSettings` event
        // was already fired.
        InitializeForPlayback(instance, *sampleRate);
    }

    SetDoubleBuffered(true);
    mTimer.SetOwner(this, timerId);
    SetSize({ minWidth, DynamicRangeProcessorPanel::graphMinHeight });
}

namespace {
double GetDisplayPixel(float elapsedSincePacket, int panelWidth)
{
    const auto secondsPerPixel
        =1. * DynamicRangeProcessorHistory::maxTimeSeconds / panelWidth;
    // A display delay to avoid the display to tremble near time zero because the
    // data hasn't arrived yet.
    // This is a trade-off between visual comfort and timely update. It was set
    // empirically, but with a relatively large audio playback delay. Maybe it
    // will be found to lag on lower-latency playbacks. Best would probably be to
    // make it playback-delay dependent.
    constexpr auto displayDelay = 0.2f;
    return panelWidth - 1
           - (elapsedSincePacket - displayDelay) / secondsPerPixel;
}

/*!
 * Wherever `A` and `B` cross, evaluates the exact x and y crossing position and
 * adds a point to `A` and `B`.
 * @pre `A.size() == B.size()`
 * @post `A.size() == B.size()`
 */
void InsertCrossings(
    std::vector<wxPoint2DDouble>& A, std::vector<wxPoint2DDouble>& B)
{
    assert(A.size() == B.size());
    if (A.size() != B.size()) {
        return;
    }
    std::optional<bool> aWasBelow;
    auto x0 = 0.;
    auto y0_a = 0.;
    auto y0_b = 0.;
    auto it = A.begin();
    auto jt = B.begin();
    while (it != A.end())
    {
        const auto x2 = it->m_x;
        const auto y2_a = it->m_y;
        const auto y2_b = jt->m_y;
        const auto aIsBelow = y2_a < y2_b;
        if (aWasBelow.has_value() && *aWasBelow != aIsBelow) {
            // clang-format off
            // We have a crossing of y2_a and y2_b between x0 and x2.
            //    y_a(x) = y0_a + (x - x0) / (x2 - x0) * (y2_a - y0_a)
            // and likewise for y_b.
            // Let y_a(x1) = y_b(x1) and solve for x1:
            // x1 = x0 + (x2 - x0) * (y0_b - y0_a) / ((a_n - y0_a) - (b_n - y0_b))
            // clang-format on
            const auto x1
                =x0 + (x2 - x0) * (y0_a - y0_b) / (y2_b - y2_a + y0_a - y0_b);
            const auto y = y0_a + (x1 - x0) / (x2 - x0) * (y2_a - y0_a);
            if (std::isfinite(x1) && std::isfinite(y)) {
                it = A.emplace(it, x1, y)++;
                jt = B.emplace(jt, x1, y)++;
            }
        }
        x0 = x2;
        y0_a = y2_a;
        y0_b = y2_b;
        aWasBelow = aIsBelow;
        ++it;
        ++jt;
    }
}

/*!
 * Fills the area between the lines and the bottom of the panel with the given
 * color.
 */
void FillUpTo(
    std::vector<wxPoint2DDouble> lines, const wxColor& color,
    wxGraphicsContext& gc, const wxRect& rect)
{
    const auto height = rect.GetHeight();
    const auto left = std::max<double>(rect.GetX(), lines.front().m_x);
    const auto right = std::min<double>(rect.GetWidth(), lines.back().m_x);
    auto area = gc.CreatePath();
    area.MoveToPoint(right, height);
    area.AddLineToPoint(left, height);
    std::for_each(lines.begin(), lines.end(), [&area](const auto& p) {
        area.AddLineToPoint(p);
    });
    area.CloseSubpath();
    gc.SetBrush(color);
    gc.FillPath(area);
}

void DrawLegend(size_t height, wxPaintDC& dc, wxGraphicsContext& gc)
{
    using namespace DynamicRangeProcessorPanel;

    constexpr auto legendWidth = 16;
    constexpr auto legendHeight = 16;
    constexpr auto legendSpacing = 8;
    constexpr auto legendX = 5;
    const auto legendY = height - 5 - legendHeight;
    const auto legendTextX = legendX + legendWidth + 5;
    const auto legendTextHeight = dc.GetTextExtent("X").GetHeight();
    const auto legendTextYOffset = (legendHeight - legendTextHeight) / 2;
    const auto legendTextY = legendY + legendTextYOffset;

    struct LegendInfo
    {
        const wxColor color;
        const TranslatableString text;
    };

    std::vector<LegendInfo> legends = {
        { inputColor, XO("Input") },
        { outputColor, XO("Output") },
    };

    int legendTextXOffset = 0;
    dc.SetTextForeground(*wxBLACK);
    dc.SetFont(
        { 10, wxFONTFAMILY_DEFAULT, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL });
    for (const auto& legend : legends) {
        // First fill with background color so that transparent foreground colors
        // yield the same result as on the graph.
        gc.SetPen(*wxTRANSPARENT_PEN);
        gc.SetBrush(backgroundColor);
        gc.DrawRectangle(
            legendX + legendTextXOffset, legendY, legendWidth, legendHeight);

        gc.SetBrush(wxColor { legend.color.GetRGB() });
        gc.DrawRectangle(
            legendX + legendTextXOffset, legendY, legendWidth, legendHeight);

        gc.SetPen(lineColor);
        gc.SetBrush(*wxTRANSPARENT_BRUSH);
        gc.DrawRectangle(
            legendX + legendTextXOffset, legendY, legendWidth, legendHeight);

        dc.DrawText(
            legend.text.Translation(), legendTextX + legendTextXOffset,
            legendTextY);
        const auto legendTextWidth
            =dc.GetTextExtent(legend.text.Translation()).GetWidth();
        legendTextXOffset += legendWidth + 5 + legendTextWidth + legendSpacing;
    }

    const auto lineY = legendY + legendHeight / 2.;
    constexpr auto lineWidth = 24;

    // Actual compression
    const auto actualX = legendX + legendTextXOffset + legendSpacing;
    const std::array<wxPoint2DDouble, 2> actualLine {
        wxPoint2DDouble(actualX, lineY),
        wxPoint2DDouble(actualX + lineWidth, lineY)
    };

    gc.SetPen({ actualCompressionColor, GetActualCompressionLineWidth() });
    gc.DrawLines(2, actualLine.data());

    if (MayUsePenGradients()) {
        wxGraphicsPenInfo penInfo;
        wxGraphicsGradientStops stops { actualCompressionColor,
                                        actualCompressionColor };
        stops.Add(attackColor.GetRGB(), 1 / 4.);
        stops.Add(actualCompressionColor, 2 / 4.);
        stops.Add(releaseColor.GetRGB(), 3 / 4.);
        penInfo.LinearGradient(actualX, 0, actualX + lineWidth, 0, stops)
        .Width(targetCompressionLineWidth);
        gc.SetPen(gc.CreatePen(penInfo));
    } else {
        gc.SetPen(actualCompressionColor);
    }

    gc.DrawLines(2, actualLine.data());
    const auto actualText = XO("Actual Compression");
    const auto actualTextX = actualX + lineWidth + legendSpacing;
    dc.DrawText(actualText.Translation(), actualTextX, legendTextY);

    // Target compression
    gc.SetPen({ targetCompressionColor, targetCompressionLineWidth });
    const auto targetX
        =actualTextX + dc.GetTextExtent(actualText.Translation()).GetWidth() + 10;
    gc.StrokeLine(targetX, lineY, targetX + lineWidth, lineY);
    const auto compressionText = XO("Target Compression");
    dc.DrawText(
        compressionText.Translation(), targetX + lineWidth + 5, legendTextY);
}
} // namespace

void DynamicRangeProcessorHistoryPanel::ShowInput(bool show)
{
    mShowInput = show;
    Refresh(false);
}

void DynamicRangeProcessorHistoryPanel::ShowOutput(bool show)
{
    mShowOutput = show;
    Refresh(false);
}

void DynamicRangeProcessorHistoryPanel::ShowActual(bool show)
{
    mShowActual = show;
    Refresh(false);
}

void DynamicRangeProcessorHistoryPanel::ShowTarget(bool show)
{
    mShowTarget = show;
    Refresh(false);
}

void DynamicRangeProcessorHistoryPanel::OnPaint(wxPaintEvent& evt)
{
    wxPaintDC dc(this);

    using namespace DynamicRangeProcessorPanel;

    const auto gc = MakeGraphicsContext(dc);
    const auto rect = DynamicRangeProcessorPanel::GetPanelRect(*this);
    const auto x = rect.GetX();
    const auto y = rect.GetY();
    const auto width = rect.GetWidth();
    const auto height = rect.GetHeight();

    gc->SetBrush(GetGraphBackgroundBrush(*gc, height));
    gc->SetPen(wxTransparentColor);
    gc->DrawRectangle(x, y, width, height);

    Finally Do { [&] {
            // The legend is causing problems color-wise, and in the end it may not be
            // so useful since the different elements of the graph can be toggled.
            // Keep it up our sleeve for now, though. (Anyone still sees this in the
            // not-so-near future, feel free to clean up.)
            constexpr auto drawLegend = false;
            if (drawLegend) {
                DrawLegend(height, dc, *gc);
            }
            gc->SetBrush(*wxTRANSPARENT_BRUSH);
            gc->SetPen(lineColor);
            gc->DrawRectangle(x, y, width, height);
        } };

    if (!mHistory || !mSync) {
        if (!mPlaybackAboutToStart) {
            const auto text = XO("awaiting playback");
            const wxDCFontChanger changer { dc,
                                            { 16, wxFONTFAMILY_DEFAULT,
                                              wxFONTSTYLE_NORMAL,
                                              wxFONTWEIGHT_NORMAL } };
            const auto textWidth = dc.GetTextExtent(text.Translation()).GetWidth();
            const auto textHeight
                =dc.GetTextExtent(text.Translation()).GetHeight();
            dc.SetTextForeground(wxColor { 128, 128, 128 });
            dc.DrawText(
                text.Translation(), (width - textWidth) / 2,
                (height - textHeight) / 2);
        }
        return;
    }

    const auto& segments = mHistory->GetSegments();
    const auto elapsedTimeSinceFirstPacket
        =std::chrono::duration<float>(mSync->now - mSync->start).count();
    const auto rangeDb = DynamicRangeProcessorPanel::GetGraphDbRange(height);
    const auto dbPerPixel = rangeDb / height;

    for (const auto& segment : segments) {
        mX.clear();
        mTarget.clear();
        mActual.clear();
        mInput.clear();
        mOutput.clear();

        mX.resize(segment.size());
        auto lastInvisibleLeft = 0;
        auto firstInvisibleRight = 0;
        std::transform(
            segment.begin(), segment.end(), mX.begin(), [&](const auto& packet) {
            const auto x = GetDisplayPixel(
                elapsedTimeSinceFirstPacket
                - (packet.time - mSync->firstPacketTime),
                width);
            if (x < 0) {
                ++lastInvisibleLeft;
            }
            if (x < width) {
                ++firstInvisibleRight;
            }
            return x;
        });
        lastInvisibleLeft = std::max<int>(--lastInvisibleLeft, 0);
        firstInvisibleRight = std::min<int>(++firstInvisibleRight, mX.size());

        mX.erase(mX.begin() + firstInvisibleRight, mX.end());
        mX.erase(mX.begin(), mX.begin() + lastInvisibleLeft);

        if (mX.size() < 2) {
            continue;
        }

        auto segmentIndex = lastInvisibleLeft;
        mTarget.reserve(mX.size());
        mActual.reserve(mX.size());
        mInput.reserve(mX.size());
        mOutput.reserve(mX.size());
        std::for_each(mX.begin(), mX.end(), [&](auto x) {
            const auto& packet = segment[segmentIndex++];
            const auto elapsedSincePacket = elapsedTimeSinceFirstPacket
                                            - (packet.time - mSync->firstPacketTime);
            mTarget.emplace_back(x, -packet.target / dbPerPixel);
            mActual.emplace_back(x, -packet.follower / dbPerPixel);
            mInput.emplace_back(x, -packet.input / dbPerPixel);
            mOutput.emplace_back(x, -packet.output / dbPerPixel);
        });

        if (mShowInput) {
            FillUpTo(mInput, inputColor, *gc, rect);
        }

        if (mShowOutput) {
            FillUpTo(mOutput, outputColor, *gc, rect);
            const auto outputGc = MakeGraphicsContext(dc);
            outputGc->SetPen({ wxColor { outputColor.GetRGB() }, 2 });
            outputGc->DrawLines(mOutput.size(), mOutput.data());
        }

        if (mShowActual) {
            // First draw a thick line, and then the thinner line in its center
            // with colors indicating overshoot and undershoot.
            const auto actualGc = MakeGraphicsContext(dc);

            actualGc->SetPen(
                wxPen { actualCompressionColor, GetActualCompressionLineWidth() });
            actualGc->DrawLines(mActual.size(), mActual.data());
            if (MayUsePenGradients()) {
                // So that we can converge to `actualCompressionColor` at the
                // crossings of actual and target compression.
                InsertCrossings(mTarget, mActual);

                wxGraphicsGradientStops stops;
                const auto xLeft = mActual.front().m_x;
                const auto xRight = mActual.back().m_x;
                const auto span = xRight - xLeft;
                for (auto i = 0; i < mActual.size(); ++i) {
                    const auto diff = mTarget[i].m_y - mActual[i].m_y;
                    const auto actualIsBelow = diff < 0;
                    const auto w = std::min(1.0, std::abs(diff) * dbPerPixel / 6);
                    const auto color = GetColorMix(
                        actualIsBelow ? releaseColor : attackColor,
                        actualCompressionColor, w)
                                       .GetRGB();
                    stops.Add(color, (mActual[i].m_x - xLeft) / span);
                }
                wxGraphicsPenInfo penInfo;
                penInfo
                .LinearGradient(
                    mActual.front().m_x, 0, mActual.back().m_x, 0, stops)
                .Width(targetCompressionLineWidth);

                actualGc->SetPen(actualGc->CreatePen(penInfo));
                actualGc->DrawLines(mActual.size(), mActual.data());
            }
        }

        if (mShowTarget) {
            const auto targetGc = MakeGraphicsContext(dc);
            targetGc->SetPen(
                wxPen { targetCompressionColor, targetCompressionLineWidth });
            targetGc->DrawLines(mTarget.size(), mTarget.data());
        }
    }
}

void DynamicRangeProcessorHistoryPanel::OnSize(wxSizeEvent& evt)
{
    Refresh(false);
    mOnDbRangeChanged(
        DynamicRangeProcessorPanel::GetGraphDbRange(GetSize().GetHeight()));
}

void DynamicRangeProcessorHistoryPanel::OnTimer(wxTimerEvent& evt)
{
    mPacketBuffer.clear();
    DynamicRangeProcessorOutputPacket packet;
    while (mOutputQueue->Get(packet)) {
        mPacketBuffer.push_back(packet);
    }
    mHistory->Push(mPacketBuffer);

    if (mHistory->IsEmpty()) {
        return;
    }

    // Do now get `std::chrono::steady_clock::now()` in the `OnPaint` event,
    // because this can be triggered even when playback is paused.
    const auto now = mClock.GetNow();
    if (!mSync) {
        // At the time of writing, the realtime playback doesn't account for
        // varying latencies. When it does, the synchronization will have to be
        // updated on latency change. See
        // https://github.com/audacity/audacity/issues/3223#issuecomment-2137025150.
        mSync.emplace(
            ClockSynchronization { mHistory->GetSegments().front().front().time
                                   + mCompressorInstance.GetLatencyMs() / 1000,
                                   now });
    }
    mPlaybackAboutToStart = false;

    mSync->now = now;

    Refresh(false);
    wxPanelWrapper::Update();
}

void DynamicRangeProcessorHistoryPanel::InitializeForPlayback(
    CompressorInstance& instance, double sampleRate)
{
    mSync.reset();
    mHistory.emplace(sampleRate);
    // We don't know for sure the least packet size (which is variable). 100
    // samples per packet at a rate of 8kHz is 12.5ms, which is quite low
    // latency. For higher sample rates that will be less.
    constexpr auto leastPacketSize = 100;
    const size_t maxQueueSize = DynamicRangeProcessorHistory::maxTimeSeconds
                                * sampleRate / leastPacketSize;
    mPacketBuffer.reserve(maxQueueSize);
    // Although `mOutputQueue` is a shared_ptr, we construct a unique_ptr and
    // invoke the shared_ptr ctor overload that takes a unique_ptr.
    // This way, we avoid the `error: aligned deallocation function of type
    // 'void (void *, std::align_val_t) noexcept' is only available on
    // macOS 10.13 or newer` compilation error.
    mOutputQueue
        =std::make_unique<DynamicRangeProcessorOutputPacketQueue>(maxQueueSize);
    instance.SetOutputQueue(mOutputQueue);
    mTimer.Start(timerPeriodMs);
    mPlaybackAboutToStart = true;
    Refresh(false);
    wxPanelWrapper::Update();
}

bool DynamicRangeProcessorHistoryPanel::AcceptsFocus() const
{
    return false;
}

bool DynamicRangeProcessorHistoryPanel::AcceptsFocusFromKeyboard() const
{
    return false;
}
