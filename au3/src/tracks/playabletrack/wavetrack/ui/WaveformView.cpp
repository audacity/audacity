/**********************************************************************

Audacity: A Digital Audio Editor

WaveformView.cpp

Paul Licameli split from WaveChannelView.cpp

**********************************************************************/

#include "WaveformView.h"

#include "WaveformAppearance.h"
#include "ClipParameters.h"
#include "WaveChannelView.h"
#include "WaveChannelViewConstants.h"
#include "WaveformVRulerControls.h"

#include "SampleHandle.h"
#include "../../../ui/EnvelopeHandle.h"
#include "../../../ui/TimeShiftHandle.h"
#include "AColor.h"
#include "Envelope.h"
#include "../../../../EnvelopeEditor.h"
#include "PendingTracks.h"
#include "../../../../ProjectSettings.h"
#include "SelectedRegion.h"
#include "SyncLock.h"
#include "../../../../TrackArt.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "../../../../TrackPanelMouseEvent.h"
#include "ViewInfo.h"
#include "WaveChannelUtilities.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveTrackUtilities.h"
#include "../../../../WaveTrackLocation.h"
#include "prefs/WaveformScale.h"
#include "WaveformSettings.h"

#include "FrameStatistics.h"

#include <wx/graphics.h>
#include <wx/dc.h>
#include <wx/gdicmn.h>

#include <wx/dcmemory.h>
#include "waveform/WaveBitmapCache.h"
#include "waveform/WaveDataCache.h"
#include "waveform/WavePaintParameters.h"

#include <atomic>

#include "prefs/GUIPrefs.h"

static WaveChannelSubView::Type sType{
    WaveChannelViewConstants::Waveform,
    { wxT("Waveform"), XXO("Wa&veform") }
};

static WaveChannelSubViewType::RegisteredType reg{ sType };

WaveformView::~WaveformView() = default;

std::vector<UIHandlePtr> WaveformView::DetailedHitTest(
    const TrackPanelMouseState& st,
    const AudacityProject* pProject, int currentTool, bool bMultiTool)
{
    auto& view = *this;
    const auto pChannel = view.FindChannel<WaveChannel>();

    auto pair = WaveChannelSubView::DoDetailedHitTest(
        st, pProject, currentTool, bMultiTool, pChannel);
    auto& results = pair.second;

    if (!pair.first) {
        UIHandlePtr result;

        if (bMultiTool) {
            // Conditional hit tests
            // If Tools toolbar were eliminated, we would keep these
            // The priority of these, in case more than one might apply at one
            // point, seems arbitrary
            if (nullptr != (result = EnvelopeHandle::WaveChannelHitTest(
                                view.mEnvelopeHandle, st.state, st.rect,
                                pProject, pChannel))) {
                results.push_back(result);
            }
            if (nullptr != (result = TimeShiftHandle::HitTest(
                                view.mTimeShiftHandle, st.state, st.rect,
                                pChannel->GetTrack().SharedPointer()))) {
                // This is the hit test on the "grips" drawn left and
                // right in Multi only
                results.push_back(result);
            }
            if (nullptr != (result = SampleHandle::HitTest(
                                view.mSampleHandle, st.state, st.rect,
                                pProject, pChannel))) {
                results.push_back(result);
            }
        } else {
            switch (currentTool) {
            // Unconditional hits appropriate to the tool
            // If tools toolbar were eliminated, we would eliminate these
            case ToolCodes::envelopeTool: {
                auto& viewInfo = ViewInfo::Get(*pProject);
                auto time
                    =viewInfo.PositionToTime(st.state.m_x, st.rect.GetX());
                const auto envelope
                    =WaveChannelUtilities::GetEnvelopeAtTime(*pChannel, time);
                result = EnvelopeHandle::HitAnywhere(
                    view.mEnvelopeHandle, envelope,
                    std::dynamic_pointer_cast<const Channel>(pChannel),
                    false);
                break;
            }
            case ToolCodes::drawTool:
                result = SampleHandle::HitAnywhere(
                    view.mSampleHandle, st.state, pChannel);
                break;
            default:
                result = {};
                break;
            }
            if (result) {
                results.push_back(result);
            }
        }
    }

    return std::move(results);
}

void WaveformView::DoSetMinimized(bool minimized)
{
    auto wt = FindWaveChannel();

    bool bHalfWave;
    gPrefs->Read(wxT("/GUI/CollapseToHalfWave"), &bHalfWave, false);
    if (wt && bHalfWave) {
        auto& cache = WaveformScale::Get(*wt);
        if (minimized) {
            // Zoom to show fractionally more than the top half of the wave.
            cache.SetDisplayBounds(-0.01f, 1.0f);
        } else {
            // Zoom out full
            cache.SetDisplayBounds(-1.0f, 1.0f);
        }
    }

    ChannelView::DoSetMinimized(minimized);
}

auto WaveformView::SubViewType() const -> const Type&
{
    return sType;
}

std::shared_ptr<ChannelVRulerControls> WaveformView::DoGetVRulerControls()
{
    return std::make_shared<WaveformVRulerControls>(shared_from_this());
}

namespace {
graphics::Color ColorFromWXPen(const wxPen& pen)
{
    const auto c = pen.GetColour();
    return graphics::Color(c.Red(), c.Green(), c.Blue());
}

graphics::Color ColorFromWXBrush(const wxBrush& brush)
{
    const auto c = brush.GetColour();
    return graphics::Color(c.Red(), c.Green(), c.Blue());
}

struct WaveBitmapCacheElementWX final : public WaveBitmapCacheElement
{
    uint8_t* Allocate(size_t width, size_t height) override
    {
        mImage = wxImage(width, height, false);
        mBitmap = wxBitmap();
        return mImage.GetData();
    }

    wxBitmap& GetBitmap()
    {
        if (!mBitmap.IsOk() && mImage.IsOk()) {
            mBitmap = wxBitmap(mImage);
        }
        return mBitmap;
    }

    size_t Width() const override
    {
        return mImage.GetWidth();
    }

    size_t Height() const override
    {
        return mImage.GetHeight();
    }

private:
    wxBitmap mBitmap;
    wxImage mImage;
};

class WaveformPainter final : public WaveClipListener
{
public:

    static WaveformPainter& Get(const WaveClip& clip);

    WaveformPainter()
    {
    }

    WaveformPainter& EnsureClip(const WaveClip& clip)
    {
        const auto changed = mChanged.exchange(false);
        if (&clip != mWaveClip || changed) {
            mChannelCaches.clear();
        }

        const auto nChannels = clip.NChannels();

        if (mChannelCaches.size() == nChannels) {
            return *this;
        }

        mWaveClip = &clip;

        mChannelCaches.reserve(nChannels);

        for (auto channelIndex = 0; channelIndex < nChannels; ++channelIndex) {
            auto dataCache = std::make_shared<WaveDataCache>(clip, channelIndex);

            auto bitmapCache = std::make_unique<WaveBitmapCache>(
                clip, dataCache,
                [] { return std::make_unique<WaveBitmapCacheElementWX>(); });

            mChannelCaches.push_back(
                { std::move(dataCache), std::move(bitmapCache) });
        }

        return *this;
    }

    void SetSelection(const ZoomInfo& zoomInfo, float t0, float t1, bool selected)
    {
        for (auto& channelCache : mChannelCaches) {
            channelCache.BitmapCache->SetSelection(zoomInfo, t0, t1, selected);
        }
    }

    void Draw(
        int channelIndex, wxDC& dc, const WavePaintParameters& params,
        const ZoomInfo& zoomInfo, const wxRect& targetRect, int leftOffset,
        double from, double to)
    {
        auto& channelCache = mChannelCaches[channelIndex];

        channelCache.BitmapCache->SetPaintParameters(params);

        auto range = channelCache.BitmapCache->PerformLookup(zoomInfo, from, to);

        auto left   = targetRect.x + leftOffset;
        auto height = targetRect.height;

        const auto top = targetRect.y;

        wxMemoryDC memdc;
        for (auto it = range.begin(); it != range.end(); ++it) {
            const auto elementLeftOffset  = it.GetLeftOffset();
            const auto elementRightOffset = it.GetRightOffset();

            const auto width = WaveBitmapCache::CacheElementWidth
                               - elementLeftOffset - elementRightOffset;

            auto& bitmap = static_cast<WaveBitmapCacheElementWX&>(*it).GetBitmap();
            memdc.SelectObject(bitmap);
            dc.Blit(
                wxPoint(left, targetRect.y), wxSize(width, it->Height()), &memdc,
                wxPoint(elementLeftOffset, 0));

            left += width;
        }
    }

    void SwapChannels() override
    {
        //Wave data cache captures channel index which becomes
        //part of it's state, request cache rebuild...
        mChanged.store(true);
    }

    void MarkChanged() noexcept override
    {
        //Triggered when any part of the waveform has changed
        //TODO: invalidate parts of the cache that intersect changes
        mChanged.store(true);
    }

    void Invalidate() override
    {
        for (auto& channelCache : mChannelCaches) {
            channelCache.DataCache->Invalidate();
            channelCache.BitmapCache->Invalidate();
        }
    }

    std::unique_ptr<WaveClipListener> Clone() const override
    {
        return std::make_unique<WaveformPainter>();
    }

private:
    const WaveClip* mWaveClip {};

    struct ChannelCaches final
    {
        std::shared_ptr<WaveDataCache> DataCache;
        std::unique_ptr<WaveBitmapCache> BitmapCache;
    };

    std::vector<ChannelCaches> mChannelCaches;
    std::atomic<bool> mChanged = false;
};

void DrawWaveform(
    TrackPanelDrawingContext& context, const WaveTrack& track, const WaveChannelInterval& channelInterval,
    int leftOffset, double t0, double t1,
    const wxRect& rect, float zoomMin, float zoomMax, bool dB, float dBRange,
    bool muted)
{
    auto& clip = channelInterval.GetClip();
    const auto channelIndex = channelInterval.GetChannelIndex();

    const auto artist = TrackArtist::Get(context);
    const ZoomInfo zoomInfo(0.0, artist->pZoomInfo->GetZoom());

    auto& clipPainter = WaveformPainter::Get(clip);

    const auto trimLeft = clip.GetTrimLeft();
    const auto sequenceStartTime = clip.GetSequenceStartTime();

    WavePaintParameters paintParameters;

    paintParameters
    .SetDisplayParameters(
        rect.GetHeight(), zoomMin, zoomMax, ShowClippingPref().Read())
    .SetDBParameters(dBRange, dB)
    .SetBlankColor(ColorFromWXBrush(artist->blankBrush))
    .SetZeroLineColor(graphics::Colors::Black)
    .SetShowRMS(ShowRMSPref().Read())
    .SetSampleColors(
        ColorFromWXPen(muted ? artist->muteSamplePen : artist->samplePen),
        ColorFromWXPen(muted ? artist->muteSamplePen : artist->samplePen))
    .SetRMSColors(
        ColorFromWXPen(muted ? artist->muteRmsPen : artist->rmsPen),
        ColorFromWXPen(muted ? artist->muteRmsPen : artist->rmsPen))
    .SetBackgroundColors(
        ColorFromWXBrush(artist->unselectedBrush),
        ColorFromWXBrush(artist->selectedBrush))
    .SetClippingColors(
        ColorFromWXPen(muted ? artist->muteClippedPen : artist->clippedPen),
        ColorFromWXPen(muted ? artist->muteClippedPen : artist->clippedPen))
    .SetEnvelope(clip.GetEnvelope());

    clipPainter.SetSelection(
        zoomInfo, artist->pSelectedRegion->t0() - sequenceStartTime,
        artist->pSelectedRegion->t1() - sequenceStartTime,
        SyncLock::IsSelectedOrSyncLockSelected(track));

    clipPainter.Draw(
        channelIndex, context.dc, paintParameters, zoomInfo, rect, leftOffset,
        t0 + trimLeft, t1 + trimLeft);
}

void DrawWaveformBackground(TrackPanelDrawingContext& context,
                            int leftOffset, const wxRect& rect,
                            const double env[],
                            float zoomMin, float zoomMax,
                            int zeroLevelYCoordinate,
                            bool dB, float dBRange,
                            double t0, double t1,
                            bool bIsSyncLockSelected,
                            bool highlightEnvelope)
{
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& zoomInfo = *artist->pZoomInfo;

    // Visually (one vertical slice of the waveform background, on its side;
    // the "*" is the actual waveform background we're drawing
    //
    //1.0                              0.0                             -1.0
    // |--------------------------------|--------------------------------|
    //      ***************                           ***************
    //      |             |                           |             |
    //    maxtop        maxbot                      mintop        minbot

    int h = rect.height;
    int halfHeight = wxMax(h / 2, 1);
    int maxtop, lmaxtop = 0;
    int mintop, lmintop = 0;
    int maxbot, lmaxbot = 0;
    int minbot, lminbot = 0;
    bool sel, lsel = false;
    int xx, lx = 0;
    int l, w;

    const auto& blankBrush = artist->blankBrush;
    const auto& selectedBrush = artist->selectedBrush;
    const auto& unselectedBrush = artist->unselectedBrush;
    const auto& envelopeBackgroundBrush = artist->envelopeBackgroundBrush;

    dc.SetPen(*wxTRANSPARENT_PEN);
    dc.SetBrush(envelopeBackgroundBrush);
    dc.DrawRectangle(rect);

    // Bug 2389 - always draw at least one pixel of selection.
    int selectedX = zoomInfo.TimeToPosition(t0, -leftOffset);

    double time = zoomInfo.PositionToTime(0, -leftOffset), nextTime;
    for (xx = 0; xx < rect.width; ++xx, time = nextTime) {
        nextTime = zoomInfo.PositionToTime(xx + 1, -leftOffset);
        // First we compute the truncated shape of the waveform background.
        // If drawEnvelope is true, then we compute the lower border of the
        // envelope.

        maxtop = GetWaveYPos(env[xx], zoomMin, zoomMax,
                             h, dB, true, dBRange, true);
        maxbot = GetWaveYPos(env[xx], zoomMin, zoomMax,
                             h, dB, false, dBRange, true);

        mintop = GetWaveYPos(-env[xx], zoomMin, zoomMax,
                             h, dB, false, dBRange, true);
        minbot = GetWaveYPos(-env[xx], zoomMin, zoomMax,
                             h, dB, true, dBRange, true);

        // Make sure it's odd so that a that max and min mirror each other
        mintop +=1;
        minbot +=1;

        const auto drawEnvelope = artist->drawEnvelope;
        if (!drawEnvelope || maxbot > mintop) {
            maxbot = halfHeight;
            mintop = halfHeight;
        }

        sel = (t0 <= time && nextTime < t1);
        sel = sel || (xx == selectedX);
        // We don't draw selection color for sync-lock selected tracks.
        sel = sel && !bIsSyncLockSelected;

        if (lmaxtop == maxtop
            && lmintop == mintop
            && lmaxbot == maxbot
            && lminbot == minbot
            && lsel == sel) {
            continue;
        }

        dc.SetBrush(lsel ? selectedBrush : unselectedBrush);

        l = rect.x + lx;
        w = xx - lx;
        if (lmaxbot < lmintop - 1) {
            dc.DrawRectangle(l, rect.y + lmaxtop, w, lmaxbot - lmaxtop);
            dc.DrawRectangle(l, rect.y + lmintop, w, lminbot - lmintop);
        } else {
            dc.DrawRectangle(l, rect.y + lmaxtop, w, lminbot - lmaxtop);
        }

        if (highlightEnvelope && lmaxbot < lmintop - 1) {
            dc.SetBrush(AColor::uglyBrush);
            dc.DrawRectangle(l, rect.y + lmaxbot, w, lmintop - lmaxbot);
        }

        lmaxtop = maxtop;
        lmintop = mintop;
        lmaxbot = maxbot;
        lminbot = minbot;
        lsel = sel;
        lx = xx;
    }

    dc.SetBrush(lsel ? selectedBrush : unselectedBrush);
    l = rect.x + lx;
    w = xx - lx;
    if (lmaxbot < lmintop - 1) {
        dc.DrawRectangle(l, rect.y + lmaxtop, w, lmaxbot - lmaxtop);
        dc.DrawRectangle(l, rect.y + lmintop, w, lminbot - lmintop);
    } else {
        dc.DrawRectangle(l, rect.y + lmaxtop, w, lminbot - lmaxtop);
    }
    if (highlightEnvelope && lmaxbot < lmintop - 1) {
        dc.SetBrush(AColor::uglyBrush);
        dc.DrawRectangle(l, rect.y + lmaxbot, w, lmintop - lmaxbot);
    }

    // If sync-lock selected, draw in linked graphics.
    if (bIsSyncLockSelected && t0 < t1) {
        const int begin = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t0, -leftOffset))));
        const int end = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t1, -leftOffset))));
        TrackArt::DrawSyncLockTiles(context,
                                    { rect.x + begin, rect.y, end - 1 - begin, rect.height });
    }

    //OK, the display bounds are between min and max, which
    //is spread across rect.height.  Draw the line at the proper place.
    if (zeroLevelYCoordinate >= rect.GetTop()
        && zeroLevelYCoordinate <= rect.GetBottom()) {
        dc.SetPen(*wxBLACK_PEN);
        AColor::Line(dc, rect.x, zeroLevelYCoordinate,
                     rect.x + rect.width - 1, zeroLevelYCoordinate);
    }
}

struct WavePortion {
    wxRect rect;
    double averageZoom;
    bool inFisheye;
    WavePortion(int x, int y, int w, int h, double zoom, bool i)
        : rect(x, y, w, h), averageZoom(zoom), inFisheye(i)
    {}
};

void FindWavePortions
    (std::vector<WavePortion>& portions, const wxRect& rect, const ZoomInfo& zoomInfo,
    const ClipParameters& params)
{
    // If there is no fisheye, then only one rectangle has nonzero width.
    // If there is a fisheye, make rectangles for before and after
    // (except when they are squeezed to zero width), and at least one for inside
    // the fisheye.

    const auto intervals = zoomInfo.FindIntervals(rect.width, rect.x);
    ZoomInfo::Intervals::const_iterator it = intervals.begin(), end = intervals.end(), prev;
    wxASSERT(it != end && it->position == rect.x);
    const int rightmost = rect.x + rect.width;
    for (int left = rect.x; left < rightmost;) {
        while (it != end && it->position <= left) {
            prev = it++;
        }
        if (it == end) {
            break;
        }
        const int right = std::max(left, (int)(it->position));
        const int width = right - left;
        if (width > 0) {
            portions.push_back(
                WavePortion(left, rect.y, width, rect.height,
                            prev->averageZoom, prev->inFisheye)
                );
        }
        left = right;
    }
}

void DrawIndividualSamples(TrackPanelDrawingContext& context,
                           int leftOffset, const wxRect& rect,
                           float zoomMin, float zoomMax,
                           bool dB, float dBRange,
                           const WaveChannelInterval& clip,
                           bool showPoints, bool muted,
                           bool highlight)
{
    const Envelope& envelope = clip.GetEnvelope();
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& zoomInfo = *artist->pZoomInfo;

    const double toffset = clip.GetPlayStartTime();
    const double rate = clip.GetRate() / clip.GetStretchRatio();
    const double t0 = std::max(0.0, zoomInfo.PositionToTime(0, -leftOffset) - toffset);
    const auto s0 = sampleCount(floor(t0 * rate));
    const auto snSamples = clip.GetVisibleSampleCount();
    if (s0 > snSamples) {
        return;
    }

    const double t1 = zoomInfo.PositionToTime(rect.width - 1, -leftOffset) - toffset;
    const auto s1 = sampleCount(ceil(t1 * rate));

    // Assume size_t will not overflow, else we wouldn't be here drawing the
    // few individual samples
    auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();

    if (slen <= 0) {
        return;
    }

    Floats buffer{ size_t(slen) };
    clip.GetSamples((samplePtr)buffer.get(), floatSample, s0, slen,
                    // Suppress exceptions in this drawing operation:
                    false);

    ArrayOf<int> xpos{ size_t(slen) };
    ArrayOf<int> ypos{ size_t(slen) };
    ArrayOf<int> clipped;
    int clipcnt = 0;

    const auto bShowClipping = ShowClippingPref().Read();
    if (bShowClipping) {
        clipped.reinit(size_t(slen));
    }

    const auto& muteSamplePen = artist->muteSamplePen;
    const auto& samplePen = artist->samplePen;
    auto& pen = highlight ? AColor::uglyPen : muted ? muteSamplePen : samplePen;
    dc.SetPen(pen);

    for (decltype(slen) s = 0; s < slen; s++) {
        const double time = toffset + (s + s0).as_double() / rate;
        const int xx // An offset into the rectangle rect
            =std::max(-10000, std::min(10000,
                                       (int)(zoomInfo.TimeToPosition(time, -leftOffset))));
        xpos[s] = xx;

        // Calculate sample as it would be rendered, so quantize time
        double value = envelope.GetValue(time, 1.0 / clip.GetRate());
        const double tt = buffer[s] * value;

        if (clipped && bShowClipping && ((tt <= -MAX_AUDIO) || (tt >= MAX_AUDIO))) {
            clipped[clipcnt++] = xx;
        }
        ypos[s]
            =std::max(-1,
                      std::min(rect.height,
                               GetWaveYPos(tt, zoomMin, zoomMax,
                                           rect.height, dB, true, dBRange, false)));
    }

    if (showPoints) {
        // Draw points where spacing is enough
        const auto bigPoints = artist->bigPoints;
        const int tickSize = bigPoints ? 4 : 3;// Bigger ellipses when draggable.
        wxRect pr;
        pr.width = tickSize;
        pr.height = tickSize;
        //different colour when draggable.
        const auto& dragsampleBrush = artist->dragsampleBrush;
        const auto& sampleBrush = artist->sampleBrush;
        auto& brush = highlight
                      ? AColor::uglyBrush
                      : bigPoints ? dragsampleBrush : sampleBrush;
        dc.SetBrush(brush);
        for (decltype(slen) s = 0; s < slen; s++) {
            if (ypos[s] >= 0 && ypos[s] < rect.height) {
                pr.x = rect.x + xpos[s] - tickSize / 2;
                pr.y = rect.y + ypos[s] - tickSize / 2;
                dc.DrawEllipse(pr);
            }
        }
    }

    const auto sampleDisplay = artist->mSampleDisplay;
    if (showPoints
        && (sampleDisplay == (int)WaveChannelViewConstants::StemPlot)
        ) {
        // Draw vertical lines
        int yZero = GetWaveYPos(0.0, zoomMin, zoomMax, rect.height, dB, true, dBRange, false);
        yZero = rect.y + std::max(-1, std::min(rect.height, yZero));
        for (decltype(slen) s = 0; s < slen; s++) {
            AColor::Line(dc,
                         rect.x + xpos[s], rect.y + ypos[s],
                         rect.x + xpos[s], yZero);
        }
    } else {
        // Connect samples with straight lines
        for (decltype(slen) s = 0; s < slen - 1; s++) {
            AColor::Line(dc,
                         rect.x + xpos[s], rect.y + ypos[s],
                         rect.x + xpos[s + 1], rect.y + ypos[s + 1]);
        }
    }

    // Draw clipping
    if (clipcnt) {
        const auto& muteClippedPen = artist->muteClippedPen;
        const auto& clippedPen = artist->clippedPen;
        dc.SetPen(muted ? muteClippedPen : clippedPen);
        while (--clipcnt >= 0) {
            auto s = clipped[clipcnt];
            AColor::Line(dc, rect.x + s, rect.y, rect.x + s, rect.y + rect.height);
        }
    }
}

void DrawEnvLine(
    TrackPanelDrawingContext& context,
    const wxRect& rect, int x0, int y0, int cy, bool top)
{
    auto& dc = context.dc;

    int xx = rect.x + x0;
    int yy = rect.y + cy;

    if (y0 < 0) {
        if (x0 % 4 != 3) {
            AColor::Line(dc, xx, yy, xx, yy + 3);
        }
    } else if (y0 > rect.height) {
        if (x0 % 4 != 3) {
            AColor::Line(dc, xx, yy - 3, xx, yy);
        }
    } else {
        if (top) {
            AColor::Line(dc, xx, yy, xx, yy + 3);
        } else {
            AColor::Line(dc, xx, yy - 3, xx, yy);
        }
    }
}

void DrawEnvelope(TrackPanelDrawingContext& context,
                  const wxRect& rect, const double env[],
                  float zoomMin, float zoomMax,
                  bool dB, float dBRange, bool highlight)
{
    auto& dc = context.dc;

    int h = rect.height;

    auto& pen = highlight ? AColor::uglyPen : AColor::envelopePen;
    dc.SetPen(pen);

    for (int x0 = 0; x0 < rect.width; ++x0) {
        int cenvTop = GetWaveYPos(env[x0], zoomMin, zoomMax,
                                  h, dB, true, dBRange, true);

        int cenvBot = GetWaveYPos(-env[x0], zoomMin, zoomMax,
                                  h, dB, true, dBRange, true);

        int envTop = GetWaveYPos(env[x0], zoomMin, zoomMax,
                                 h, dB, true, dBRange, false);

        int envBot = GetWaveYPos(-env[x0], zoomMin, zoomMax,
                                 h, dB, true, dBRange, false);

        // Make the collision at zero actually look solid
        if (cenvBot - cenvTop < 9) {
            int value = (int)((zoomMax / (zoomMax - zoomMin)) * h);
            cenvTop = value - 4;
            cenvBot = value + 4;
        }

        DrawEnvLine(context, rect, x0, envTop, cenvTop, true);
        DrawEnvLine(context, rect, x0, envBot, cenvBot, false);
    }
}

// Headers needed only for experimental drawing below
//#include "tracks/playabletrack/wavetrack/ui/SampleHandle.h"
//#include "tracks/ui/EnvelopeHandle.h"
void DrawClipWaveform(TrackPanelDrawingContext& context,
                      const WaveChannel& channel, const WaveChannelInterval& clip,
                      const wxRect& rect, bool dB, bool muted, bool selected)
{
    const Envelope& envelope = clip.GetEnvelope();
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);
    const auto& selectedRegion = *artist->pSelectedRegion;
    const auto& zoomInfo = *artist->pZoomInfo;

    auto sw = FrameStatistics::CreateStopwatch(FrameStatistics::SectionID::WaveformView);

    bool highlightEnvelope = false;

    //If clip is "too small" draw a placeholder instead of
    //attempting to fit the contents into a few pixels
    if (!WaveChannelView::ClipDetailsVisible(clip, zoomInfo, rect)) {
        auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
        TrackArt::DrawClipFolded(dc, clipRect);
        return;
    }

    const ClipParameters params { clip, rect, zoomInfo };
    const wxRect& hiddenMid = params.hiddenMid;
    // The "hiddenMid" rect contains the part of the display actually
    // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
    if (hiddenMid.width <= 0) {
        return;
    }

    const double& t0 = params.t0;
    const double& t1 = params.t1;
    const double playStartTime = clip.GetPlayStartTime();
    const double& trackRectT0 = params.trackRectT0;
    const double& averagePixelsPerSecond = params.averagePixelsPerSecond;
    const double sampleRate = clip.GetRate();
    const double stretchRatio = clip.GetStretchRatio();
    double leftOffset = params.leftOffset;
    const wxRect& mid = params.mid;

    auto& settings = WaveformSettings::Get(channel);
    const float dBRange = settings.dBRange;

    dc.SetPen(*wxTRANSPARENT_PEN);
    int iColorIndex = WaveColorAttachment::Get(clip).GetColorIndex();
    artist->SetColours(iColorIndex);

    // The bounds (controlled by vertical zooming; -1.0...1.0
    // by default)
    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(channel);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    std::vector<double> vEnv(mid.width);
    double* const env = &vEnv[0];
    CommonChannelView::GetEnvelopeValues(envelope, playStartTime,

                                         // PRL: change back to make envelope evaluate only at sample times
                                         // and then interpolate the display
                                         0, // 1.0 / sampleRate,

                                         env, mid.width, leftOffset, zoomInfo);

    // Draw the background of the track, outlining the shape of
    // the envelope and using a colored pen for the selected
    // part of the waveform
    {
        double tt0, tt1;
        const auto& track = channel.GetTrack();
        if (SyncLock::IsSelectedOrSyncLockSelected(track)) {
            tt0 = track.SnapToSample(selectedRegion.t0());
            tt1 = track.SnapToSample(selectedRegion.t1());
        } else {
            tt0 = tt1 = 0.0;
        }
        DrawWaveformBackground(context, leftOffset, mid,
                               env,
                               zoomMin, zoomMax,
                               cache.ZeroLevelYCoordinate(mid),
                               dB, dBRange,
                               tt0, tt1,
                               !track.GetSelected(), highlightEnvelope);
    }

    // Require at least 1/2 pixel per sample for drawing individual samples.
    const double threshold1 = 0.5 * sampleRate / stretchRatio;
    // Require at least 3 pixels per sample for drawing the draggable points.
    const double threshold2 = 3 * sampleRate / stretchRatio;

    bool highlight = false;

    const bool showIndividualSamples = zoomInfo.GetZoom() > threshold1;
    const bool showPoints = zoomInfo.GetZoom() > threshold2;

    if (!showIndividualSamples) {
        DrawWaveform(
            context, channel.GetTrack(), clip, leftOffset, t0, t1,
            rect, zoomMin, zoomMax, dB, dBRange, muted);
    } else {
        std::vector<WavePortion> portions;
        FindWavePortions(portions, rect, zoomInfo, params);
        auto offset = leftOffset;
        for (const auto& portion : portions) {
            assert(!portion.inFisheye && portion.averageZoom > threshold1);
            if (portion.inFisheye || portion.averageZoom <= threshold1) {
                continue;
            }

            wxRect rectPortion = portion.rect;
            rectPortion.Intersect(mid);
            DrawIndividualSamples(
                context, offset, rectPortion, zoomMin, zoomMax, dB, dBRange, clip,
                showPoints, muted, highlight);
            offset += rectPortion.width;
        }
    }

    const auto drawEnvelope = artist->drawEnvelope;
    if (drawEnvelope) {
        DrawEnvelope(
            context, mid, env, zoomMin, zoomMax, dB, dBRange, highlightEnvelope);
        EnvelopeEditor::DrawPoints(envelope,
                                   context, mid, dB, dBRange, zoomMin, zoomMax, true, rect.x - mid.x);
    }

    // Draw arrows on the left side if the track extends to the left of the
    // beginning of time.  :)
    if (trackRectT0 == 0.0 && playStartTime < 0.0) {
        TrackArt::DrawNegativeOffsetTrackArrows(context, rect);
    }
    {
        auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
        TrackArt::DrawClipEdges(dc, clipRect, selected);
    }
}

void DrawTimeSlider(TrackPanelDrawingContext& context,
                    const wxRect& rect,
                    bool rightwards, bool highlight)
{
    auto& dc = context.dc;

    const int border = 3; // 3 pixels all round.
    const int width = 6; // width of the drag box.
    const int taper = 6; // how much the box tapers by.
    const int barSpacing = 4; // how far apart the bars are.
    const int barWidth = 3;
    const int xFlat = 3;

    //Enough space to draw in?
    if (rect.height <= ((taper + border + barSpacing) * 2)) {
        return;
    }
    if (rect.width <= (width * 2 + border * 3)) {
        return;
    }

    // The draggable box is tapered towards the direction you drag it.
    int leftTaper  = rightwards ? 0 : 6;
    int rightTaper = rightwards ? 6 : 0;

    int xLeft = rightwards ? (rect.x + border - 2)
                : (rect.x + rect.width + 1 - (border + width));
    int yTop  = rect.y + border;
    int yBot  = rect.y + rect.height - border - 1;

    AColor::Light(&dc, false, highlight);
    AColor::Line(dc, xLeft,         yBot - leftTaper, xLeft,         yTop + leftTaper);
    AColor::Line(dc, xLeft,         yTop + leftTaper, xLeft + xFlat, yTop);
    AColor::Line(dc, xLeft + xFlat, yTop,             xLeft + width, yTop + rightTaper);

    AColor::Dark(&dc, false, highlight);
    AColor::Line(dc, xLeft + width,         yTop + rightTaper, xLeft + width,       yBot - rightTaper);
    AColor::Line(dc, xLeft + width,         yBot - rightTaper, xLeft + width - xFlat, yBot);
    AColor::Line(dc, xLeft + width - xFlat, yBot,              xLeft,               yBot - leftTaper);

    int firstBar = yTop + taper + taper / 2;
    int nBars    = (yBot - yTop - taper * 3) / barSpacing + 1;
    xLeft += (width - barWidth + 1) / 2;
    int yy;
    int i;

    AColor::Light(&dc, false, highlight);
    for (i = 0; i < nBars; i++) {
        yy = firstBar + barSpacing * i;
        AColor::Line(dc, xLeft, yy, xLeft + barWidth, yy);
    }
    AColor::Dark(&dc, false, highlight);
    for (i = 0; i < nBars; i++) {
        yy = firstBar + barSpacing * i + 1;
        AColor::Line(dc, xLeft, yy, xLeft + barWidth, yy);
    }
}
}

// Header needed only for experimental drawing below
//#include "tracks/ui/TimeShiftHandle.h"
void WaveformView::DoDraw(TrackPanelDrawingContext& context,
                          const WaveChannel& channel,
                          const WaveTrack::Interval* selectedClip,
                          const wxRect& rect,
                          bool muted)
{
    auto& dc = context.dc;
    const auto artist = TrackArtist::Get(context);

    bool highlight = false;
    bool gripHit = false;

    const bool dB = !WaveformSettings::Get(channel).isLinear();

    const auto& blankSelectedBrush = artist->blankSelectedBrush;
    const auto& blankBrush = artist->blankBrush;
    TrackArt::DrawBackgroundWithSelection(
        context, rect, channel, blankSelectedBrush, blankBrush);

    for (const auto& pInterval : channel.Intervals()) {
        bool selected = selectedClip
                        && selectedClip == &pInterval->GetClip();
        DrawClipWaveform(context, channel, *pInterval, rect, dB, muted, selected);
    }
    DrawBoldBoundaries(context, channel, rect);

    const auto drawSliders = artist->drawSliders;
    if (drawSliders) {
        DrawTimeSlider(context, rect, true, highlight && gripHit);  // directed right
        DrawTimeSlider(context, rect, false, highlight && gripHit); // directed left
    }
}

void WaveformView::Draw(
    TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass)
{
    if (iPass == TrackArtist::PassTracks) {
        const auto artist = TrackArtist::Get(context);
        const auto& pendingTracks = *artist->pPendingTracks;
        auto& dc = context.dc;

        const auto pChannel = FindChannel();
        if (!pChannel) {
            return;
        }
        const auto& wc = static_cast<const WaveChannel&>(
            pendingTracks.SubstitutePendingChangedChannel(*pChannel));

        const auto hasSolo = artist->hasSolo;
        bool muted = (hasSolo || wc.GetTrack().GetMute())
                     && !wc.GetTrack().GetSolo();

#if defined(__WXMAC__)
        wxAntialiasMode aamode = dc.GetGraphicsContext()->GetAntialiasMode();
        dc.GetGraphicsContext()->SetAntialiasMode(wxANTIALIAS_NONE);
#endif

        auto waveChannelView = GetWaveChannelView().lock();
        wxASSERT(waveChannelView.use_count());

        auto selectedClip = waveChannelView->GetSelectedClip();
        DoDraw(context, wc, selectedClip.get(), rect, muted);

#if defined(__WXMAC__)
        dc.GetGraphicsContext()->SetAntialiasMode(aamode);
#endif
    }
    WaveChannelSubView::Draw(context, rect, iPass);
}

static const WaveChannelSubViews::RegisteredFactory key{
    [](WaveChannelView& view) {
        return std::make_shared<WaveformView>(view);
    }
};

// The following attaches the wave color sub-menu to the wave track popup
// menu.  It is appropriate only to waveform view and so is kept in this
// source file with the rest of the waveform view implementation.

#include <mutex> // for std::call_once
#include "WaveTrackControls.h"
#include "../../../../widgets/PopupMenuTable.h"
#include "ProjectAudioIO.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"

//=============================================================================
// Table class for a sub-menu
struct WaveColorMenuTable : PopupMenuTable
{
    WaveColorMenuTable()
        : PopupMenuTable{"WaveColor", XO("&Wave Color")} {}
    DECLARE_POPUP_MENU(WaveColorMenuTable);

    static WaveColorMenuTable& Instance();

    void InitUserData(void* pUserData) override;

    PlayableTrackControls::InitMenuData* mpData{};

    int IdOfWaveColor(int WaveColor);
    void OnWaveColorChange(wxCommandEvent& event);

    int OnInstrument1ID, OnInstrument2ID, OnInstrument3ID, OnInstrument4ID;
};

WaveColorMenuTable& WaveColorMenuTable::Instance()
{
    static WaveColorMenuTable instance;
    return instance;
}

void WaveColorMenuTable::InitUserData(void* pUserData)
{
    mpData = static_cast<PlayableTrackControls::InitMenuData*>(pUserData);
}

namespace {
using ValueFinder = std::function< int (WaveTrack&) >;

const TranslatableString GetWaveColorStr(int colorIndex)
{
    return XXO("Instrument %i").Format(colorIndex + 1);
}
}

BEGIN_POPUP_MENU(WaveColorMenuTable)
static const auto fn = []( PopupMenuHandler& handler, wxMenu& menu, int id ){
    auto& me = static_cast<WaveColorMenuTable&>(handler);
    auto pData = me.mpData;
    const auto& track = static_cast<WaveTrack&>(pData->track);
    auto& project = pData->project;
    bool unsafe = ProjectAudioIO::Get(project).IsAudioActive();

    menu.Check(id, id == me.IdOfWaveColor(
                   WaveformAppearance::Get(track).GetColorIndex()));
    menu.Enable(id, !unsafe);
};

static std::once_flag flag;
std::call_once(flag, [this]{
    auto& hostTable = GetWaveTrackMenuTable();
    OnInstrument1ID = hostTable.ReserveId();
    OnInstrument2ID = hostTable.ReserveId();
    OnInstrument3ID = hostTable.ReserveId();
    OnInstrument4ID = hostTable.ReserveId();
});

AppendRadioItem("Instrument1", OnInstrument1ID,
                GetWaveColorStr(0), POPUP_MENU_FN(OnWaveColorChange), fn);
AppendRadioItem("Instrument2", OnInstrument2ID,
                GetWaveColorStr(1), POPUP_MENU_FN(OnWaveColorChange), fn);
AppendRadioItem("Instrument3", OnInstrument3ID,
                GetWaveColorStr(2), POPUP_MENU_FN(OnWaveColorChange), fn);
AppendRadioItem("Instrument4", OnInstrument4ID,
                GetWaveColorStr(3), POPUP_MENU_FN(OnWaveColorChange), fn);

END_POPUP_MENU()

/// Converts a WaveColor enumeration to a wxWidgets menu item Id.
int WaveColorMenuTable::IdOfWaveColor(int WaveColor)
{ return OnInstrument1ID + WaveColor; }

/// Handles the selection from the WaveColor submenu of the
/// track menu.
void WaveColorMenuTable::OnWaveColorChange(wxCommandEvent& event)
{
    int id = event.GetId();
    wxASSERT(id >= OnInstrument1ID && id <= OnInstrument4ID);
    auto& track = static_cast<WaveTrack&>(mpData->track);

    int newWaveColor = id - OnInstrument1ID;

    AudacityProject* const project = &mpData->project;

    WaveformAppearance::Get(track).SetColorIndex(newWaveColor);

    ProjectHistory::Get(*project)
    .PushState(XO("Changed '%s' to %s")
               .Format(track.GetName(), GetWaveColorStr(newWaveColor)),
               XO("WaveColor Change"));

    using namespace RefreshCode;
    mpData->result = RefreshAll | FixScrollbars;
}

namespace {
PopupMenuTable::AttachedItem sAttachment{
    GetWaveTrackMenuTable(),
    { "SubViews/Extra" },
    std::make_unique<PopupMenuSection>("WaveColor",
                                       // Conditionally add sub-menu for wave color, if showing waveform
                                       PopupMenuTable::Adapt<WaveTrackPopupMenuTable>(
                                           [](WaveTrackPopupMenuTable& table) {
        const auto pTrack = &table.FindWaveTrack();
        const auto& view = WaveChannelView::GetFirst(*pTrack);
        const auto displays = view.GetDisplays();
        bool hasWaveform = (displays.end() != std::find(
                                displays.begin(), displays.end(),
                                WaveChannelSubView::Type {
                WaveChannelViewConstants::Waveform, {} }
                                ));
        return hasWaveform
               ? Registry::Indirect(WaveColorMenuTable::Instance()
                                    .Get(table.mpData))
               : nullptr;
    }))
};
}

static WaveClip::Attachments::RegisteredFactory sKeyW{ [](WaveClip&) {
        return std::make_unique<WaveformPainter>();
    } };

WaveformPainter& WaveformPainter::Get(const WaveClip& clip)
{
    return const_cast< WaveClip& >(clip)  // Consider it mutable data
           .Attachments::Get<WaveformPainter>(sKeyW).EnsureClip(clip);
}
