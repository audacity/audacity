#include "au3wavepainter.h"

#include <QColor>
#include <QPainter>
#include <QPen>

#include <wx/types.h>
#include <wx/utils.h>

#include "ClipInterface.h"
#include "CodeConversions.h"
#include "SelectedRegion.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "ZoomInfo.h"
#include "Envelope.h"
#include "FrameStatistics.h"
#include "SyncLock.h"
#include "ViewInfo.h"
#include "WaveformScale.h"
#include "graphics/Color.h"

#include "waveform/WaveBitmapCache.h"
#include "waveform/WaveDataCache.h"

#include "domaccessor.h"

constexpr int kClipDetailedViewMinimumWidth{ 3 };

constexpr int FrameRadius { 4 };
constexpr int HeaderHeight { 0 };
constexpr int HeaderVMargin { 2 };

using Style = au::au3::Au3WavePainter::Style;

namespace WaveChannelViewConstants {
// Only two types of sample display for now, but
// others (eg sinc interpolation) may be added later.
enum SampleDisplay {
    LinearInterpolate = 0,
    StemPlot
};
}

namespace {
class WaveformSettings final : public ClientData::Cloneable<>
{
public:
    //! Create waveform settings for the track on demand
    //! Mutative access to attachment even if the track argument is const
    static WaveformSettings& Get(const WaveTrack& track);

    static WaveformSettings& defaults();

    PointerType Clone() const override;

    bool isLinear() const { return true; }

    float dBRange{ 60.0 };
};

struct WaveBitmapCacheElementQt final : public WaveBitmapCacheElement
{
    uint8_t* Allocate(size_t width, size_t height) override
    {
        mWidth = width;
        mHeight = height;

        mBytes.resize(std::max(mWidth * mHeight * 3, mBytes.size()));
        return mBytes.data();
    }

    QImage GetImage() const
    {
        return QImage((const uchar*)mBytes.data(), mWidth, mHeight, mWidth * 3, QImage::Format_RGB888);
    }

    size_t Width() const override
    {
        return mWidth;
    }

    size_t Height() const override
    {
        return mHeight;
    }

private:
    size_t mWidth{};
    size_t mHeight{};
    std::vector<uint8_t> mBytes;
};

class WaveformPainter final : public WaveClipListener
{
public:

    static WaveformPainter& Get(const WaveClip& cache);

    WaveformPainter& EnsureClip(const WaveClip& clip)
    {
        if (&clip != mWaveClip) {
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
                [] { return std::make_unique<WaveBitmapCacheElementQt>(); });

            mChannelCaches.push_back(
                { std::move(dataCache), std::move(bitmapCache) });
        }

        return *this;
    }

    void Draw(int channelIndex,
              QPainter& painter, const
              WavePaintParameters& params,
              const ZoomInfo& zoomInfo,
              const QRect& targetRect,
              int leftOffset,
              double from,
              double to)
    {
        assert(channelIndex >= 0 && channelIndex < mChannelCaches.size());
        if (channelIndex < 0 || channelIndex >= mChannelCaches.size()) {
            return;
        }

        auto& bitmapCache = mChannelCaches[channelIndex].BitmapCache;
        bitmapCache->SetPaintParameters(params);

        auto range = bitmapCache->PerformLookup(zoomInfo, from, to);

        auto left = targetRect.x() + leftOffset;
        auto height = targetRect.height();

        LOGDA() << "zoom: " << zoomInfo.GetZoom()
                << " leftOffset: " << leftOffset
                << " targetRect.x() : " << targetRect.x()
                << " left: " << left
                << " from: " << from
                << " to: " << to;

        const auto top = targetRect.y();

        for (auto it = range.begin(); it != range.end(); ++it) {
            const auto elementLeftOffset = it.GetLeftOffset();
            const auto elementRightOffset = it.GetRightOffset();

            const auto width = WaveBitmapCache::CacheElementWidth
                               - elementLeftOffset - elementRightOffset;

            const auto drawableWidth = std::min<int32_t>(
                width, it->Width() - elementLeftOffset);

            const auto image = static_cast<const WaveBitmapCacheElementQt&>(*it).GetImage();
            painter.drawImage(
                QRectF(left, targetRect.y(), drawableWidth, height),
                image,
                QRectF(
                    elementLeftOffset,
                    0,
                    std::clamp(drawableWidth, 0, image.width() - static_cast<int>(elementLeftOffset)),
                    std::clamp(height, 0, image.height())
                    )
                );

            left += width;
        }
    }

    void MarkChanged() noexcept override { }

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
};
}

static const ChannelGroup::Attachments::RegisteredFactory
    key1{ [](auto&) {
        return std::make_unique<WaveformSettings>(WaveformSettings::defaults());
    } };

WaveformSettings& WaveformSettings::Get(const WaveTrack& track)
{
    auto& mutTrack = const_cast<WaveTrack&>(track);
    return mutTrack.Attachments::Get<WaveformSettings>(key1);
}

WaveformSettings& WaveformSettings::defaults()
{
    static WaveformSettings instance;
    return instance;
}

auto WaveformSettings::Clone() const -> PointerType
{
    return std::make_unique<WaveformSettings>(*this);
}

static WaveClip::Attachments::RegisteredFactory sKeyW{ [](WaveClip&) {
        return std::make_unique<WaveformPainter>();
    } };

WaveformPainter& WaveformPainter::Get(const WaveClip& clip)
{
    return const_cast< WaveClip& >(clip)   // Consider it mutable data
           .Attachments::Get<WaveformPainter>(sKeyW).EnsureClip(clip);
}

bool ShowIndividualSamples(
    int sampleRate, double stretchRatio, double pixelsPerSecond)
{
    const auto secondsPerSample = stretchRatio / sampleRate;
    const auto pixelsPerSample = pixelsPerSecond * secondsPerSample;
    return pixelsPerSample > 0.5;
}

double GetPixelsPerSecond(const QRect& viewRect, const ZoomInfo& zoomInfo)
{
    const auto h = zoomInfo.PositionToTime(0, 0, true);
    const auto trackRectT1 = zoomInfo.PositionToTime(viewRect.width(), 0, true);
    return viewRect.width() / (trackRectT1 - h);
}

double GetBlankSpaceBeforePlayEndTime(const ClipTimes& clip)
{
    return 0.99 * clip.GetStretchRatio() / clip.GetRate();
}

static graphics::Color ColorFromQColor(const QColor& color)
{
    return graphics::Color(color.red(), color.green(), color.blue(), color.alpha());
}

// Returns an offset in seconds to be applied to the right clip
// boundary so that it does not overlap the last sample
double CalculateAdjustmentForZoomLevel(double avgPixPerSecond, bool showSamples)
{
    constexpr double pixelsOffset { 2 }; // The desired offset in pixels
    if (showSamples) {
        // adjustment so that the last circular point doesn't appear
        // to be hanging off the end
        return pixelsOffset
               / avgPixPerSecond; // pixels / ( pixels / second ) = seconds
    }
    return .0;
}

void GetEnvelopeValues(const Envelope& env,
                       double alignedTime, double sampleDur,
                       double* buffer, int bufferLen, int leftOffset,
                       const ZoomInfo& zoomInfo)
{
    // Getting many envelope values, corresponding to pixel columns, which may
    // not be uniformly spaced in time when there is a fisheye.

    double prevDiscreteTime=0.0, prevSampleVal=0.0, nextSampleVal=0.0;
    for (int xx = 0; xx < bufferLen; ++xx) {
        auto time = zoomInfo.PositionToTime(xx, -leftOffset);
        if (sampleDur <= 0) {
            // Sample interval not defined (as for time track)
            buffer[xx] = env.GetValue(time);
        } else {
            // The level of zoom-in may resolve individual samples.
            // If so, then instead of evaluating the envelope directly,
            // we draw a piecewise curve with knees at each sample time.
            // This actually makes clearer what happens as you drag envelope
            // points and make discontinuities.
            auto leftDiscreteTime = alignedTime
                                    + sampleDur * floor((time - alignedTime) / sampleDur);
            if (xx == 0 || leftDiscreteTime != prevDiscreteTime) {
                prevDiscreteTime = leftDiscreteTime;
                prevSampleVal
                    =env.GetValue(prevDiscreteTime, sampleDur);
                nextSampleVal
                    =env.GetValue(prevDiscreteTime + sampleDur, sampleDur);
            }
            auto ratio = (time - leftDiscreteTime) / sampleDur;
            if (env.GetExponential()) {
                buffer[ xx ] = exp(
                    (1.0 - ratio) * log(prevSampleVal)
                    + ratio * log(nextSampleVal));
            } else {
                buffer[ xx ]
                    =(1.0 - ratio) * prevSampleVal + ratio * nextSampleVal;
            }
        }
    }
}

/// Takes a value between min and max and returns a value between
/// height and 0
int GetWaveYPos(float value, float min, float max,
                int height, bool dB, bool outer,
                float dBr, bool clip)
{
    if (dB) {
        if (height == 0) {
            return 0;
        }

        float sign = (value >= 0 ? 1 : -1);

        if (value != 0.) {
            float db = LINEAR_TO_DB(fabs(value));
            value = (db + dBr) / dBr;
            if (!outer) {
                value -= 0.5;
            }
            if (value < 0.0) {
                value = 0.0;
            }
            value *= sign;
        }
    } else {
        if (!outer) {
            if (value >= 0.0) {
                value -= 0.5;
            } else {
                value += 0.5;
            }
        }
    }

    if (clip) {
        if (value < min) {
            value = min;
        }
        if (value > max) {
            value = max;
        }
    }

    value = (max - value) / (max - min);
    return (int)(value * (height - 1) + 0.5);
}

struct ClipParameters
{
    // Do a bunch of calculations common to waveform and spectrum drawing.
    ClipParameters(const ClipTimes& clip, const QRect& rect, const ZoomInfo& zoomInfo);

    const double trackRectT0; // absolute time of left edge of track

    // Lower and upper visible time boundaries (relative to clip). If completely
    // off-screen, `t0 == t1`.
    double t0;
    double t1;

    const double averagePixelsPerSecond;
    const bool showIndividualSamples;

    QRect hiddenMid;
    int hiddenLeftOffset;

    QRect mid;
    int leftOffset;

    // returns a clip rectangle restricted by viewRect,
    // and with clipOffsetX - clip horizontal origin offset within view rect
    static QRect GetClipRect(
        const ClipTimes& clip, const ZoomInfo& zoomInfo, const QRect& viewRect, bool* outShowSamples = nullptr);
};

ClipParameters::ClipParameters(
    const ClipTimes& clip, const QRect& rect, const ZoomInfo& zoomInfo)
    : trackRectT0{zoomInfo.PositionToTime(0, 0, true)}
    , averagePixelsPerSecond{GetPixelsPerSecond(rect, zoomInfo)}
    , showIndividualSamples{ShowIndividualSamples(
                                clip.GetRate(), clip.GetStretchRatio(), averagePixelsPerSecond)}
{
    const auto trackRectT1 = zoomInfo.PositionToTime(rect.width(), 0, true);
    const auto playStartTime = clip.GetPlayStartTime();

    const double clipLength = clip.GetPlayEndTime() - clip.GetPlayStartTime();

    // Hidden duration because too far left.
    const auto tpre = trackRectT0 - playStartTime;
    const auto tpost = trackRectT1 - playStartTime;

    const auto blank = GetBlankSpaceBeforePlayEndTime(clip);

    // Calculate actual selection bounds so that t0 > 0 and t1 < the
    // end of the track
    t0 = std::max(tpre, .0);
    t1 = std::min(tpost, clipLength - blank)
         + CalculateAdjustmentForZoomLevel(
        averagePixelsPerSecond, showIndividualSamples);

    // Make sure t1 (the right bound) is greater than 0
    if (t1 < 0.0) {
        t1 = 0.0;
    }

    // Make sure t1 is greater than t0
    if (t0 > t1) {
        t0 = t1;
    }

    // The variable "hiddenMid" will be the rectangle containing the
    // actual waveform, as opposed to any blank area before
    // or after the track, as it would appear without the fisheye.
    hiddenMid = rect;

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "hiddenMid"
    hiddenLeftOffset = 0;
    if (tpre < 0) {
        // Fix Bug #1296 caused by premature conversion to (int).
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime, 0, true);
        if (time64 < 0) {
            time64 = 0;
        }
        hiddenLeftOffset = (time64 < rect.width()) ? (int)time64 : rect.width();

        hiddenMid.setLeft(hiddenMid.left() + hiddenLeftOffset);
    }

    // If the right edge of the track is to the left of the right
    // edge of the display, then there's some unused area to the right
    // of the track.  Reduce the "hiddenMid" rect by the
    // size of the blank area.
    if (tpost > t1) {
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime + t1, 0, true);
        if (time64 < 0) {
            time64 = 0;
        }
        const int hiddenRightOffset = (time64 < rect.width()) ? (int)time64 : rect.width();

        hiddenMid.setWidth(std::max(0, hiddenRightOffset - hiddenLeftOffset));
    }
    // The variable "mid" will be the rectangle containing the
    // actual waveform, as distorted by the fisheye,
    // as opposed to any blank area before or after the track.
    mid = rect;

    // If the left edge of the track is to the right of the left
    // edge of the display, then there's some unused area to the
    // left of the track.  Reduce the "mid"
    leftOffset = 0;
    if (tpre < 0) {
        wxInt64 time64 = 0;//zoomInfo.TimeToPosition(playStartTime, 0, false);
        if (time64 < 0) {
            time64 = 0;
        }
        leftOffset = (time64 < rect.width()) ? (int)time64 : rect.width();

        mid.setLeft(mid.left() + leftOffset);
    }

    // If the right edge of the track is to the left of the right
    // edge of the display, then there's some unused area to the right
    // of the track.  Reduce the "mid" rect by the
    // size of the blank area.
    if (tpost > t1) {
        wxInt64 time64 = zoomInfo.TimeToPosition(playStartTime + t1, 0, false);
        if (time64 < 0) {
            time64 = 0;
        }
        const int distortedRightOffset = (time64 < rect.width()) ? (int)time64 : rect.width();

        mid.setWidth(std::max(0, distortedRightOffset - leftOffset));
    }
}

QRect ClipParameters::GetClipRect(const ClipTimes& clip,
                                  const ZoomInfo& zoomInfo, const QRect& viewRect, bool* outShowSamples)
{
    const auto pixelsPerSecond = GetPixelsPerSecond(viewRect, zoomInfo);
    const auto showIndividualSamples = ShowIndividualSamples(
        clip.GetRate(), clip.GetStretchRatio(), pixelsPerSecond);
    const auto clipEndingAdjustment
        =CalculateAdjustmentForZoomLevel(pixelsPerSecond, showIndividualSamples);
    if (outShowSamples != nullptr) {
        *outShowSamples = showIndividualSamples;
    }
    constexpr auto edgeLeft
        =static_cast<ZoomInfo::int64>(std::numeric_limits<int>::min());
    constexpr auto edgeRight
        =static_cast<ZoomInfo::int64>(std::numeric_limits<int>::max());
    const auto left = std::clamp(
        zoomInfo.TimeToPosition(clip.GetPlayStartTime(), viewRect.x(), true),
        edgeLeft, edgeRight);
    const auto right = std::clamp(
        zoomInfo.TimeToPosition(
            clip.GetPlayEndTime() - GetBlankSpaceBeforePlayEndTime(clip)
            + clipEndingAdjustment,
            viewRect.x(), true),
        edgeLeft, edgeRight);
    if (right >= left) {
        // after clamping we can expect that left and right
        // are small enough to be put into int
        return {
            static_cast<int>(left),
            viewRect.y(),
            std::max(1, static_cast<int>(right - left)),
            viewRect.height()
        };
    }
    return {};
}

void DrawIndividualSamples(int channelIndex, QPainter& painter, const QRect& rect,
                           const Style& style,
                           const ZoomInfo& zoomInfo,
                           const WaveClip& clip,
                           int leftOffset,
                           float zoomMin, float zoomMax,
                           bool dB, float dBRange,
                           bool showPoints, bool muted, bool highlight)
{
    UNUSED(muted);

    const double toffset = clip.GetPlayStartTime();
    double rate = clip.GetRate();
    const double t0 = std::max(0.0, zoomInfo.PositionToTime(0, -leftOffset) - toffset);
    const auto s0 = sampleCount(floor(t0 * rate));
    const auto snSamples = clip.GetVisibleSampleCount();
    if (s0 > snSamples) {
        return;
    }

    const double t1 = zoomInfo.PositionToTime(rect.width() - 1, -leftOffset) - toffset;
    const auto s1 = sampleCount(ceil(t1 * rate));

    // Assume size_t will not overflow, else we wouldn't be here drawing the
    // few individual samples
    auto slen = std::min(snSamples - s0, s1 - s0 + 1).as_size_t();

    if (slen <= 0) {
        return;
    }

    Floats buffer{ size_t(slen) };
    clip.GetSamples(channelIndex, (samplePtr)buffer.get(), floatSample, s0, slen,
                    // Suppress exceptions in this drawing operation:
                    false);

    ArrayOf<int> xpos{ size_t(slen) };
    ArrayOf<int> ypos{ size_t(slen) };
    ArrayOf<int> clipped;
    int clipcnt = 0;

    //TODO: uncomment and fix
    //const auto bShowClipping = artist->mShowClipping;
    const auto bShowClipping = false;
    if (bShowClipping) {
        clipped.reinit(size_t(slen));
    }

    painter.setPen(highlight ? style.highlight : style.samplePen);

    for (decltype(slen) s = 0; s < slen; s++) {
        const double time = toffset + (s + s0).as_double() / rate;
        const int xx   // An offset into the rectangle rect
            =std::max(-10000, std::min(10000,
                                       (int)(zoomInfo.TimeToPosition(time, -leftOffset))));
        xpos[s] = xx;

        // Calculate sample as it would be rendered, so quantize time
        double value
            =clip.GetEnvelope().GetValue(time, 1.0 / clip.GetRate());
        const double tt = buffer[s] * value;

        if (clipped && bShowClipping && ((tt <= -MAX_AUDIO) || (tt >= MAX_AUDIO))) {
            clipped[clipcnt++] = xx;
        }
        ypos[s]
            =std::max(-1,
                      std::min(rect.height(),
                               GetWaveYPos(tt, zoomMin, zoomMax,
                                           rect.height(), dB, true, dBRange, false)));
    }

    if (showPoints) {
        // Draw points where spacing is enough
        //TODO: uncomment and fix
        //const auto bigPoints = artist->bigPoints;
        const auto bigPoints = false;
        const int tickSize = bigPoints ? 4 : 3;// Bigger ellipses when draggable.
        auto pr = QRect(0, 0, tickSize, tickSize);

        //maybe need different colour when draggable.
        auto brush = highlight ? style.highlight : (bigPoints ? style.sampleBrush : style.sampleBrush);

        painter.setBrush(brush);

        for (decltype(slen) s = 0; s < slen; s++) {
            if (ypos[s] >= 0 && ypos[s] < rect.height()) {
                pr.moveLeft(rect.x() + xpos[s] - tickSize / 2);
                pr.moveTop(rect.y() + ypos[s] - tickSize / 2);

                //painter.drawEllipse(pr);
            }
        }
    }

    //TODO: uncomment and fix
    //const auto sampleDisplay = artist->mSampleDisplay;
    const auto sampleDisplay = true;
    if (showPoints && (sampleDisplay == (int)WaveChannelViewConstants::StemPlot)) {
        // Draw vertical lines
        int yZero = GetWaveYPos(0.0, zoomMin, zoomMax, rect.height(), dB, true, dBRange, false);
        yZero = rect.y() + std::max(-1, std::min(rect.height(), yZero));
        for (decltype(slen) s = 0; s < slen; s++) {
            painter.drawLine(
                rect.x() + xpos[s], rect.y() + ypos[s],
                rect.x() + xpos[s], yZero);
        }
    } else {
        // Connect samples with straight lines
        for (decltype(slen) s = 0; s < slen - 1; s++) {
            painter.drawLine(
                rect.x() + xpos[s], rect.y() + ypos[s],
                rect.x() + xpos[s + 1], rect.y() + ypos[s + 1]);
        }
    }

    // Draw clipping
    if (clipcnt) {
        painter.setPen(style.clippedPen);
        while (--clipcnt >= 0) {
            auto s = clipped[clipcnt];
            painter.drawLine(rect.x() + s, rect.y(), rect.x() + s, rect.y() + rect.height());
        }
    }
}

void DrawWaveformBackground(QPainter& painter, const QRect& rect,
                            const Style& style,
                            const ZoomInfo& zoomInfo,
                            const double* env, int leftOffset,
                            float zoomMin, float zoomMax,
                            double t0, double t1,
                            bool dB, float dBRange,
                            int zeroLevelYCoordinate,
                            bool bIsSyncLockSelected,
                            bool highlightEnvelope)
{
    // Visually (one vertical slice of the waveform background, on its side;
    // the "*" is the actual waveform background we're drawing
    //
    //1.0                              0.0                             -1.0
    // |--------------------------------|--------------------------------|
    //      ***************                           ***************
    //      |             |                           |             |
    //    maxtop        maxbot                      mintop        minbot

    int h = rect.height();
    int halfHeight = wxMax(h / 2, 1);
    int maxtop, lmaxtop = 0;
    int mintop, lmintop = 0;
    int maxbot, lmaxbot = 0;
    int minbot, lminbot = 0;
    bool sel, lsel = false;
    int xx, lx = 0;
    int l, w;

    painter.setPen(Qt::NoPen);
    painter.setBrush(style.blankBrush);

    painter.drawRect(rect);

    // Bug 2389 - always draw at least one pixel of selection.
    int selectedX = zoomInfo.TimeToPosition(t0, -leftOffset);

    double time = zoomInfo.PositionToTime(0, -leftOffset), nextTime;
    for (xx = 0; xx < rect.width(); ++xx, time = nextTime) {
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

        //TODO: uncomment and fix
        //const auto drawEnvelope = artist->drawEnvelope;
        const auto drawEnvelope = false;
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

        painter.setBrush(style.blankBrush);

        l = rect.x() + lx;
        w = xx - lx;
        if (lmaxbot < lmintop - 1) {
            painter.drawRect(l, rect.y() + lmaxtop, w, lmaxbot - lmaxtop);
            painter.drawRect(l, rect.y() + lmintop, w, lminbot - lmintop);
        } else {
            painter.drawRect(l, rect.y() + lmaxtop, w, lminbot - lmaxtop);
        }

        if (highlightEnvelope && lmaxbot < lmintop - 1) {
            painter.setBrush(style.highlight);
            painter.drawRect(l, rect.y() + lmaxbot, w, lmintop - lmaxbot);
        }

        lmaxtop = maxtop;
        lmintop = mintop;
        lmaxbot = maxbot;
        lminbot = minbot;
        lsel = sel;
        lx = xx;
    }

    painter.setBrush(style.blankBrush);
    l = rect.x() + lx;
    w = xx - lx;
    if (lmaxbot < lmintop - 1) {
        painter.drawRect(l, rect.y() + lmaxtop, w, lmaxbot - lmaxtop);
        painter.drawRect(l, rect.y() + lmintop, w, lminbot - lmintop);
    } else {
        painter.drawRect(l, rect.y() + lmaxtop, w, lminbot - lmaxtop);
    }
    if (highlightEnvelope && lmaxbot < lmintop - 1) {
        painter.setBrush(style.highlight);
        painter.drawRect(l, rect.y() + lmaxbot, w, lmintop - lmaxbot);
    }

    //TODO: uncomment and fix
    // If sync-lock selected, draw in linked graphics.
    /*if (bIsSyncLockSelected && t0 < t1) {
      const int begin = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t0, -leftOffset))));
      const int end = std::max(0, std::min(rect.width, (int)(zoomInfo.TimeToPosition(t1, -leftOffset))));
      TrackArt::DrawSyncLockTiles( context,
         { rect.x + begin, rect.y, end - 1 - begin, rect.height } );
   }*/

    //OK, the display bounds are between min and max, which
    //is spread across rect.height.  Draw the line at the proper place.
    if (zeroLevelYCoordinate >= rect.top()
        && zeroLevelYCoordinate <= rect.bottom()) {
        painter.setPen(Qt::black);
        painter.drawLine(rect.x(), zeroLevelYCoordinate,
                         rect.x() + rect.width() - 1, zeroLevelYCoordinate);
    }
}

void DrawMinMaxRMS(int channelIndex, QPainter& painter,
                   const QRect& rect,
                   const Style& style,
                   const ZoomInfo& zoomInfo,
                   const WaveClip& clip,
                   double t0, double t1, int leftOffset,
                   double zoomMin, double zoomMax,
                   bool dB, double dbRange,
                   bool muted)
{
    UNUSED(muted);

    const ZoomInfo localZoomInfo(0.0, zoomInfo.GetZoom());

    auto& waveformPainter = WaveformPainter::Get(clip);

    const auto trimLeft = clip.GetTrimLeft();

    WavePaintParameters paintParameters;

    paintParameters
    .SetDisplayParameters(
        //TODO: uncomment and fix
        rect.height(), zoomMin, zoomMax, false /*artist->mShowClipping*/)
    .SetDBParameters(dbRange, dB)
    .SetBlankColor(ColorFromQColor(style.blankBrush))
    .SetSampleColors(
        ColorFromQColor(style.samplePen),
        ColorFromQColor(style.samplePen))
    .SetRMSColors(
        ColorFromQColor(style.rmsPen),
        ColorFromQColor(style.rmsPen))
    .SetBackgroundColors(
        ColorFromQColor(style.blankBrush),
        ColorFromQColor(style.blankBrush))
    .SetClippingColors(
        ColorFromQColor(style.clippedPen),
        ColorFromQColor(style.clippedPen))
    .SetEnvelope(clip.GetEnvelope());

    //TODO: uncomment and fix
    //clipPainter.SetSelection(
    //   zoomInfo, artist->pSelectedRegion->t0() - sequenceStartTime,
    //   artist->pSelectedRegion->t1() - sequenceStartTime);

    waveformPainter.Draw(channelIndex, painter, paintParameters, localZoomInfo, rect, leftOffset, t0 + trimLeft, t1 + trimLeft);
}

static bool ClipDetailsVisible(const ClipTimes& clip, const ZoomInfo& zoomInfo, const QRect& viewRect)
{
    //Do not fold clips to line at sample zoom level, as
    //it may become impossible to 'unfold' it when clip is trimmed
    //to a single sample
    bool showSamples{ false };
    auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, viewRect, &showSamples);
    return showSamples || clipRect.width() >= kClipDetailedViewMinimumWidth;
}

void DrawWaveform(int channelIndex,
                  QPainter& painter,
                  WaveTrack& track,
                  const WaveClip& clip,
                  const ZoomInfo& zoomInfo,
                  const SelectedRegion& selectedRegion,
                  const QRect& rect,
                  const Style& style,
                  bool dB, bool muted, bool selected)
{
    //If clip is "too small" draw a placeholder instead of
    //attempting to fit the contents into a few pixels
    if (!ClipDetailsVisible(clip, zoomInfo, rect)) {
        //TODO: uncomment and fix me
        /*
      auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
      TrackArt::DrawClipFolded(context.painter, clipRect);
      */
        return;
    }

    bool highlightEnvelope = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
    auto target = dynamic_cast<EnvelopeHandle*>(context.target.get());
    highlightEnvelope = target && target->GetEnvelope() == clip.GetEnvelope();
#endif

    const ClipParameters params(clip, rect, zoomInfo);
    const auto& hiddenMid = params.hiddenMid;
    // The "hiddenMid" rect contains the part of the display actually
    // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
    if (hiddenMid.width() <= 0) {
        return;
    }

    const double t0 = params.t0;
    LOGDA() << "t0: " << t0;
    const double sampleRate = clip.GetRate();
    const double playStartTime = clip.GetPlayStartTime();
    const double trackRectT0 = params.trackRectT0;
    const double stretchRatio = clip.GetStretchRatio();
    const double t1 = params.t1;
    const int leftOffset = params.leftOffset;
    const QRect mid = params.mid;

    auto& settings = WaveformSettings::Get(track);
    const float dBRange = settings.dBRange;

    painter.setPen(Qt::NoPen);

    // The bounds (controlled by vertical zooming; -1.0...1.0
    // by default)
    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    std::vector<double> vEnv(mid.width());
    double* const env = &vEnv[0];
    GetEnvelopeValues(
        clip.GetEnvelope(),
        playStartTime,

        // PRL: change back to make envelope evaluate only at sample times
        // and then interpolate the display
        0,         // 1.0 / sampleRate,

        env, mid.width(), leftOffset, zoomInfo
        );

    // Draw the background of the track, outlining the shape of
    // the envelope and using a colored pen for the selected
    // part of the waveform
    {
        double tt0, tt1;
        if (SyncLock::IsSelectedOrSyncLockSelected(track)) {
            tt0 = track.LongSamplesToTime(track.TimeToLongSamples(selectedRegion.t0())),
            tt1 = track.LongSamplesToTime(track.TimeToLongSamples(selectedRegion.t1()));
        } else {
            tt0 = tt1 = 0.0;
        }

        DrawWaveformBackground(
            painter, mid,
            style,
            zoomInfo,
            env, leftOffset,
            zoomMin, zoomMax,
            tt0, tt1,
            dB, dBRange,
            cache.ZeroLevelYCoordinate({ mid.x(), mid.y(), mid.width(), mid.height() }),
            !track.GetSelected(), highlightEnvelope);
    }

    //const double pps =
    //   averagePixelsPerSample * rate;

    // Require at least 1/2 pixel per sample for drawing individual samples.
    const double threshold1 = 0.5 * sampleRate / stretchRatio;
    // Require at least 3 pixels per sample for drawing the draggable points.
    const double threshold2 = 3 * sampleRate / stretchRatio;

    const bool showIndividualSamples = zoomInfo.GetZoom() > threshold1;
    const bool showPoints = zoomInfo.GetZoom() > threshold2;

    if (!showIndividualSamples) {
        DrawMinMaxRMS(channelIndex,
                      painter, rect,
                      style,
                      zoomInfo,
                      clip,
                      t0, t1, leftOffset,
                      zoomMin, zoomMax,
                      dB, dBRange, muted);
    } else {
        bool highlight = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
        auto target = dynamic_cast<SampleHandle*>(context.target.get());
        highlight = target && target->GetTrack().get() == track;
#endif
        DrawIndividualSamples(
            channelIndex,
            painter, rect,
            style,
            zoomInfo,
            clip, leftOffset,
            zoomMin, zoomMax,
            dB, dBRange,
            showPoints, muted, highlight);
    }

    //TODO: uncomment and fix me
    /*const auto drawEnvelope = artist->drawEnvelope;
   if (drawEnvelope) {
      DrawEnvelope(
         context, mid, env, zoomMin, zoomMax, dB, dBRange, highlightEnvelope );
      EnvelopeEditor::DrawPoints( *clip->GetEnvelope(),
          context, mid, dB, dBRange, zoomMin, zoomMax, true, rect.x - mid.x );
   }*/

    // Draw arrows on the left side if the track extends to the left of the
    // beginning of time.  :)
    //TODO: uncomment and fix me
    /*if (trackRectT0 == 0.0 && playStartTime < 0.0) {
      TrackArt::DrawNegativeOffsetTrackArrows( context, rect );
   }
   {
      auto clipRect = ClipParameters::GetClipRect(*clip, zoomInfo, rect);
      TrackArt::DrawClipEdges(context.painter, clipRect, selected);
   }*/
}

using namespace au::au3;

AudacityProject& Au3WavePainter::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

void Au3WavePainter::paint(QPainter& painter, const processing::ClipKey& clipKey, const Params& params)
{
    LOGD() << "trackId: " << clipKey.trackId << ", clip: " << clipKey.index;
    WaveTrack* track = DomAccessor::findWaveTrack(projectRef(), TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> clip = DomAccessor::findWaveClip(track, clipKey.index);
    IF_ASSERT_FAILED(clip) {
        return;
    }

    doPaint(painter, track, clip.get(), params);
}

void Au3WavePainter::doPaint(QPainter& painter, const WaveTrack* _track, const WaveClip* clip, const Params& params)
{
    // debug
    // const auto& vr = params.viewRect;
    // LOGDA() << "viewRect: w: " << vr.width() << ", h: " << vr.height() << ", x: " << vr.x() << ", y: " << vr.y();

    auto sw = FrameStatistics::CreateStopwatch(FrameStatistics::SectionID::WaveformView);

    WaveTrack* track = const_cast<WaveTrack*>(_track);

    bool highlightEnvelope = false;
#ifdef EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING
    auto target = dynamic_cast<EnvelopeHandle*>(context.target.get());
    highlightEnvelope = target && target->GetEnvelope() == clip->GetEnvelope();
#endif

    const bool dB = !WaveformSettings::Get(*track).isLinear();

    const auto channelHeight = (params.geometry.height) / static_cast<int>(clip->NChannels());

    const Geometry& g = params.geometry;

    double width = std::min(g.left + g.width, g.frameLeft + g.frameWidth) - g.left;
    double left = std::min(g.left - g.frameLeft, 0.0);
    double clipStartTime = clip->GetPlayStartTime();// + (left / params.zoom);
    LOGDA() << "g.width: " << g.width
            << " g.left: " << g.left
            << " g.frameWidth: " << g.frameWidth
            << " g.frameLeft: " << g.frameLeft
            << " (g.left + g.width): " << (g.left + g.width)
            << " (g.frameLeft + g.frameWidth): " << (g.frameLeft + g.frameWidth)
            << " width: " << width
            << " left: " << left;

    ZoomInfo zoomInfo(clipStartTime, params.zoom);
    SelectedRegion selectedRegion{};
    double top = 0.0;
    for (unsigned i = 0; i < clip->NChannels(); ++i) {
        QRect rect(0, top, width, channelHeight);
        DrawWaveform(i, painter, *track, *clip, zoomInfo, selectedRegion, rect, params.style, dB, track->GetMute(), false);
        top += channelHeight;
    }
}
