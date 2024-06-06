#include "au3wavepainter.h"

#include <QColor>
#include <QPainter>
#include <QPen>

#include <wx/types.h>
#include <wx/utils.h>

#include "ClipInterface.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "ZoomInfo.h"
#include "Envelope.h"
#include "FrameStatistics.h"
#include "WaveformScale.h"
#include "graphics/Color.h"

#include "waveform/WaveBitmapCache.h"
#include "waveform/WaveDataCache.h"

#include "domaccessor.h"

constexpr double CLIPVIEW_WIDTH_MIN = 4; // px

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

            mChannelCaches.push_back({ std::move(dataCache), std::move(bitmapCache) });
        }

        return *this;
    }

    struct Geometry
    {
        double top = 0.0;
        double left = 0.0;
        double height = 0.0;
        double width = 0.0;    // not used for draw, just info
    };

    void Draw(int channelIndex,
              QPainter& painter, const
              WavePaintParameters& params,
              const Geometry& geometry,
              double zoom,
              double from,
              double to)
    {
        assert(channelIndex >= 0 && channelIndex < mChannelCaches.size());
        if (channelIndex < 0 || channelIndex >= mChannelCaches.size()) {
            return;
        }

        auto& bitmapCache = mChannelCaches[channelIndex].BitmapCache;
        bitmapCache->SetPaintParameters(params);

        const ZoomInfo zoomInfo(0.0, zoom);

        auto range = bitmapCache->PerformLookup(zoomInfo, from, to);

        int left = geometry.left;
        int height = geometry.height;

        for (auto it = range.begin(); it != range.end(); ++it) {
            const auto elementLeftOffset = it.GetLeftOffset();
            const auto elementRightOffset = it.GetRightOffset();

            const auto width = WaveBitmapCache::CacheElementWidth - elementLeftOffset - elementRightOffset;

            const auto drawableWidth = std::min<int32_t>(width, it->Width() - elementLeftOffset);

            const auto image = static_cast<const WaveBitmapCacheElementQt&>(*it).GetImage();
            painter.drawImage(
                QRectF(left, geometry.top, drawableWidth, height),
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
               / avgPixPerSecond;   // pixels / ( pixels / second ) = seconds
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

struct WaveGeometry
{
    double waveTop = 0.0;       // wave channel view top
    double waveHeight = 0.0;    // wave channel view height
    double clipWidth = 0.0;     // clip view width
    double relClipLeft = 0.0;   // relatively to frameLeft
    double frameLeft = 0.0;     // track line shift
    double frameWidth = 0.0;    // track line visible width
};

struct ClipParameters
{
    // Do a bunch of calculations common to waveform and spectrum drawing.
    ClipParameters(const WaveGeometry& geometry, double zoom);

    const WaveGeometry geometry;

    WaveformPainter::Geometry drawGeometry;

    // Lower and upper visible time boundaries (relative to clip). If completely
    // off-screen, `t0 == t1`.
    double t0 = 0.0;
    double t1 = 0.0;
};

ClipParameters::ClipParameters(const WaveGeometry& geomet, double zoom)
    : geometry(geomet)
{
    double drawWidth = std::min(geometry.relClipLeft + geometry.clipWidth, geometry.frameWidth) - geometry.relClipLeft;
    double drawLeft = 0.0;
    if (geometry.relClipLeft < 0) {
        drawLeft = -geometry.relClipLeft;
    }

    drawGeometry.top = geometry.waveTop;
    drawGeometry.left = drawLeft;
    drawGeometry.height = geometry.waveHeight;
    drawGeometry.width = drawWidth;

    t0 = drawLeft / zoom;
    t1 = drawWidth / zoom;

    // LOGDA() << " relClipLeft: " << geometry.relClipLeft
    //         << " clipWidth: " << geometry.clipWidth
    //         << " frameLeft: " << geometry.frameLeft
    //         << " frameWidth: " << geometry.frameWidth
    //         << " draw width: " << drawWidth
    //         << " draw left: " << drawLeft
    //         << " t0: " << t0
    //         << " t1: " << t1
    // ;
}

void DrawIndividualSamples(int channelIndex, QPainter& painter, const QRect& rect,
                           const Style& style,
                           const ZoomInfo& zoomInfo,
                           const WaveClip& clip,
                           int leftOffset,
                           float zoomMin, float zoomMax,
                           bool dB, float dBRange,
                           bool showPoints, bool highlight)
{
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

void DrawMinMaxRMS(int channelIndex, QPainter& painter,
                   const ClipParameters& params,
                   double zoom,
                   const Style& style,
                   const WaveClip& clip,
                   double zoomMin, double zoomMax,
                   bool dB, double dbRange)
{
    auto& waveformPainter = WaveformPainter::Get(clip);

    WavePaintParameters paintParameters;

    paintParameters
    .SetDisplayParameters(
        //TODO: uncomment and fix
        params.drawGeometry.height, zoomMin, zoomMax, false /*artist->mShowClipping*/)
    .SetDBParameters(dbRange, dB)
    .SetBlankColor(ColorFromQColor(style.blankBrush))
    .SetSampleColors(
        ColorFromQColor(style.samplePen),
        ColorFromQColor(style.samplePen))
    .SetShowRMS(false)
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

    const auto trimLeft = clip.GetTrimLeft();

    waveformPainter.Draw(channelIndex, painter, paintParameters, params.drawGeometry, zoom, params.t0 + trimLeft, params.t1 + trimLeft);
}

static bool showIndividualSamples(const WaveClip& clip, bool zoom)
{
    const double sampleRate = clip.GetRate();
    const double stretchRatio = clip.GetStretchRatio();

    // Require at least 1/2 pixel per sample for drawing individual samples.
    const double threshold1 = 0.5 * sampleRate / stretchRatio;

    bool showIndividualSamples = zoom > threshold1;
    return showIndividualSamples;
}

static void DrawWaveform(int channelIndex,
                         QPainter& painter,
                         WaveTrack& track,
                         const WaveClip& clip,
                         const WaveGeometry& geometry,
                         double zoom,
                         const Style& style,
                         bool dB)
{
    //If clip is "too small" draw a placeholder instead of
    //attempting to fit the contents into a few pixels
    if (geometry.clipWidth < CLIPVIEW_WIDTH_MIN) {
        //TODO: uncomment and fix me
        /*
      auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
      TrackArt::DrawClipFolded(context.painter, clipRect);
      */
        return;
    }

    const ClipParameters params(geometry, zoom);
    if (params.drawGeometry.width < 0.1) {
        return;
    }

    auto& settings = WaveformSettings::Get(track);
    const float dBRange = settings.dBRange;

    painter.setPen(Qt::NoPen);

    // The bounds (controlled by vertical zooming; -1.0...1.0
    // by default)
    float zoomMin, zoomMax;
    auto& cache = WaveformScale::Get(track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    if (!showIndividualSamples(clip, zoom)) {
        DrawMinMaxRMS(channelIndex, painter,
                      params, zoom,
                      style,
                      clip,
                      zoomMin, zoomMax,
                      dB, dBRange);
    } else {
        // Require at least 3 pixels per sample for drawing the draggable points.
        // const double threshold2 = 3 * sampleRate / stretchRatio;
        // const bool showPoints =zoom > threshold2;
        // bool highlight = false;
        // DrawIndividualSamples(
        //     channelIndex,
        //     painter, rect,
        //     style,
        //     zoomInfo,
        //     clip, leftOffset,
        //     zoomMin, zoomMax,
        //     dB, dBRange,
        //     showPoints, highlight);
    }
}

using namespace au::au3;

AudacityProject& Au3WavePainter::projectRef() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return *project;
}

void Au3WavePainter::paint(QPainter& painter, const processing::ClipKey& clipKey, const Params& params)
{
    //! NOTE Please don't remove, need for debug
    // if (!(clipKey.trackId == 2 && clipKey.index == 0)) {
    //     return;
    // }
    // LOGD() << "trackId: " << clipKey.trackId << ", clip: " << clipKey.index;

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
    auto sw = FrameStatistics::CreateStopwatch(FrameStatistics::SectionID::WaveformView);

    WaveTrack* track = const_cast<WaveTrack*>(_track);

    const bool dB = !WaveformSettings::Get(*track).isLinear();

    const auto channelHeight = (params.geometry.clipHeight) / static_cast<int>(clip->NChannels());

    const Geometry& g = params.geometry;

    WaveGeometry cg;
    cg.waveHeight = channelHeight;
    cg.clipWidth = g.clipWidth;
    cg.relClipLeft = g.relClipLeft;
    cg.frameLeft = g.frameLeft;
    cg.frameWidth = g.frameWidth;

    cg.waveTop = 0.0;
    for (unsigned i = 0; i < clip->NChannels(); ++i) {
        DrawWaveform(i, painter, *track, *clip, cg, params.zoom, params.style, dB);
        cg.waveTop += channelHeight;
    }
}
