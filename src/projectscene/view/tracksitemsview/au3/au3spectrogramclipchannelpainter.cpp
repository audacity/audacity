/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramclipchannelpainter.h"

#include "./ClipParameters.h"
#include "./SpectrumCache.h"
#include "./wavepainterutils.h" // TODO generalize
#include "../../../internal/au3/viewinfo.h"

#include "framework/global/log.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-screen-geometry/NumberScale.h"
#include "libraries/lib-theme/AColor.h"

namespace au::projectscene {
using Au3SelectedRegion = ::SelectedRegion;

namespace {
std::pair<sampleCount, sampleCount> GetSelectedSampleIndices(
    const Au3SelectedRegion& selectedRegion, const WaveChannelInterval& clip,
    bool trackIsSelected)
{
    if (!trackIsSelected) {
        return { 0, 0 };
    }
    const double t0 = selectedRegion.t0(); // left selection bound
    const double t1 = selectedRegion.t1(); // right selection bound
    const auto startTime = clip.GetPlayStartTime();
    const auto s0 = std::max(sampleCount(0), clip.TimeToSamples(t0 - startTime));
    auto s1 = std::clamp(
        clip.TimeToSamples(t1 - startTime), sampleCount { 0 },
        clip.GetVisibleSampleCount());
    return { s0, s1 };
}

float findValue(const float* spectrum, float bin0, float bin1, unsigned nBins, bool autocorrelation, int gain, int range)
{
    float value;

    // Maximum method, and no apportionment of any single bins over multiple pixel rows
    // See Bug971
    int index, limitIndex;
    if (autocorrelation) {
        // bin = 2 * nBins / (nBins - 1 - array_index);
        // Solve for index
        index = std::max(0.0f, std::min(float(nBins - 1),
                                        (nBins - 1) - (2 * nBins) / (std::max(1.0f, bin0))
                                        ));
        limitIndex = std::max(0.0f, std::min(float(nBins - 1),
                                             (nBins - 1) - (2 * nBins) / (std::max(1.0f, bin1))
                                             ));
    } else {
        index = std::min<int>(nBins - 1, (int)(floor(0.5 + bin0)));
        limitIndex = std::min<int>(nBins, (int)(floor(0.5 + bin1)));
    }
    value = spectrum[index];
    while (++index < limitIndex) {
        value = std::max(value, spectrum[index]);
    }

    if (!autocorrelation) {
        // Last step converts dB to a 0.0-1.0 range
        value = (value + range + gain) / (double)range;
    }
    value = std::min(1.0f, std::max(0.0f, value));
    return value;
}

constexpr auto DASH_LENGTH = 10; // pixel

inline AColor::ColorGradientChoice
ChooseColorSet(float bin0, float bin1, float selBinLo,
               float selBinCenter, float selBinHi, int dashCount, bool isSpectral)
{
    if (!isSpectral) {
        return AColor::ColorGradientTimeSelected;
    }
    if ((selBinCenter >= 0) && (bin0 <= selBinCenter)
        && (selBinCenter < bin1)) {
        return AColor::ColorGradientEdge;
    }
    if ((0 == dashCount % 2)
        && (((selBinLo >= 0) && (bin0 <= selBinLo) && (selBinLo < bin1))
            || ((selBinHi >= 0) && (bin0 <= selBinHi) && (selBinHi < bin1)))) {
        return AColor::ColorGradientEdge;
    }
    if ((selBinLo < 0 || selBinLo < bin1) && (selBinHi < 0 || selBinHi > bin0)) {
        return AColor::ColorGradientTimeAndFrequencySelected;
    }

    return AColor::ColorGradientTimeSelected;
}
}

Au3SpectrogramClipChannelPainter::Au3SpectrogramClipChannelPainter(std::shared_ptr<WaveClipChannel> channel)
    : m_waveClipChannel{std::move(channel)} {}

void Au3SpectrogramClipChannelPainter::paint(QPainter& painter, const SpectrogramGlobalContext& gc, const SpectrogramTrackContext& tc)
{
    SpectrogramSettings &settings = tc.settings;
    const ZoomInfo &zoomInfo = gc.zoomInfo;
    auto& clipChannel = *m_waveClipChannel;
    Au3SelectedRegion selectedRegion;
    selectedRegion.setT0(gc.selectedRegion.t0);
    selectedRegion.setT1(gc.selectedRegion.t1);
    selectedRegion.setF0(gc.selectedRegion.f0);
    selectedRegion.setF1(gc.selectedRegion.f1);

    const auto paintableRectHeight = static_cast<int>(gc.metrics.height); // TODO is this correct?
    const QRect paintableRect{ 0, 0, zoomInfo.viewportWidth(), paintableRectHeight };
    const ClipParameters clipParams { clipChannel, paintableRect, zoomInfo };

    const QRect& paintableClipRect = clipParams.paintableClipRect;
    // The "paintableClipRect" rect contains the part of the display actually
    // containing the waveform, or the intersection of the track's paintable rect with the clip.
    // If it's empty, we're done.
    if (paintableClipRect.width() <= 0) {
        return;
    }

    const double& visibleT0 = clipParams.visibleT0;
    const double playStartTime = clipChannel.GetPlayStartTime();

    const auto [ssel0, ssel1] = GetSelectedSampleIndices(selectedRegion, clipChannel, tc.trackIsSelected);
    const double& averagePixelsPerSecond = clipParams.averagePixelsPerSecond;
    const double sampleRate = clipChannel.GetRate();
    const double stretchRatio = clipChannel.GetStretchRatio();
    const double& leftOffset = clipParams.leftOffset;

    double freqLo = SelectedRegion::UndefinedFrequency;
    double freqHi = SelectedRegion::UndefinedFrequency;
    freqLo = selectedRegion.f0();
    freqHi = selectedRegion.f1();

    const int& colorScheme = settings.colorScheme;
    const int& range = settings.range;
    const int& gain = settings.gain;

    QImage image(paintableClipRect.width(), paintableClipRect.height(), QImage::Format_RGB888);

    const float* spectrogram = nullptr;
    const sampleCount* where = nullptr;
    const auto half = settings.GetFFTLength() / 2;
    const double binUnit = sampleRate / (2 * half);
    const bool updated = WaveClipSpectrumCache::Get(clipChannel).GetSpectrogram(clipChannel, spectrogram, settings, where,
                                                                                paintableClipRect.width(), visibleT0, averagePixelsPerSecond);

    auto nBins = settings.NBins();

    const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

    // nearest frequency to each pixel row from number scale, for selecting
    // the desired fft bin(s) for display on that row
    float* bins = (float*)alloca(sizeof(*bins) * (paintableClipRect.height() + 1));
    {
        const NumberScale numberScale(settings.GetScale(tc.minFreq, tc.maxFreq));

        NumberScale::Iterator it = numberScale.begin(paintableClipRect.height());
        float nextBin = std::max(0.0f, std::min(float(nBins - 1),
                                                settings.findBin(*it, binUnit)));

        int yy;
        for (yy = 0; yy < paintableClipRect.height(); ++yy) {
            bins[yy] = nextBin;
            nextBin = std::max(0.0f, std::min(float(nBins - 1),
                                              settings.findBin(*++it, binUnit)));
        }
        bins[yy] = nextBin;
    }

    const bool autocorrelation = settings.algorithm == SpectrogramSettings::algPitchEAC;
    auto& clipCache = WaveClipSpectrumCache::Get(clipChannel);
    auto& specPxCache = clipCache.mSpecPxCaches[clipChannel.GetChannelIndex()];
    if (!updated && specPxCache
        && ((int)specPxCache->len == paintableClipRect.height() * paintableClipRect.width())
        && scaleType == specPxCache->scaleType
        && gain == specPxCache->gain
        && range == specPxCache->range
        && tc.minFreq == specPxCache->minFreq
        && tc.maxFreq == specPxCache->maxFreq
        ) {
        // Wave clip's spectrum cache is up to date,
        // and so is the spectrum pixel cache
    } else {
        // Update the spectrum pixel cache
        specPxCache = std::make_unique<SpecPxCache>(paintableClipRect.width() * paintableClipRect.height());
        specPxCache->scaleType = scaleType;
        specPxCache->gain = gain;
        specPxCache->range = range;
        specPxCache->minFreq = tc.minFreq;
        specPxCache->maxFreq = tc.maxFreq;

        for (int xx = 0; xx < paintableClipRect.width(); ++xx) {
            for (int yy = 0; yy < paintableClipRect.height(); ++yy) {
                const float bin     = bins[yy];
                const float nextBin = bins[yy + 1];
                const float value = findValue(spectrogram + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
                specPxCache->values[xx * paintableClipRect.height() + yy] = value;
            }
        }
    } // updating cache

    float selBinLo = settings.findBin(freqLo, binUnit);
    float selBinHi = settings.findBin(freqHi, binUnit);
    float selBinCenter = (freqLo < 0 || freqHi < 0)
                         ? -1
                         : settings.findBin(sqrt(freqLo * freqHi), binUnit);

    const bool isSpectral = settings.SpectralSelectionEnabled();
    constexpr int begin = 0;
    constexpr int end = 0;
    constexpr size_t numPixels = std::max(0, end - begin);

    SpecCache specCache;

    // need explicit resize since specCache.where[] accessed before Populate()
    specCache.Grow(numPixels, settings, -1, visibleT0);

    if (numPixels > 0) {
        for (int ii = begin; ii < end; ++ii) {
            const double time = zoomInfo.PositionToTime(ii, -leftOffset) - playStartTime;
            specCache.where[ii - begin]
                =sampleCount(0.5 + sampleRate / stretchRatio * time);
        }
        // TODO why 0 pixels per second?
        specCache.Populate(settings, clipChannel, 0, 0, numPixels, 0);
    }

    // build color gradient tables (not thread safe)
    if (!AColor::gradient_inited) {
        AColor::PreComputeGradient();
    }

    // Bug 2389 - always draw at least one pixel of selection.
    int selectedX = zoomInfo.TimeToPosition(selectedRegion.t0(), -leftOffset);

    const NumberScale numberScale(settings.GetScale(tc.minFreq, tc.maxFreq));
    const int windowSize = settings.WindowSize();

    // Lambda for converting yy (not mouse coord!) to respective freq. bins
    auto yyToFreqBin = [&](int yy){
        const double p = double(yy) / paintableClipRect.height();
        float convertedFreq = numberScale.PositionToValue(p);
        float convertedFreqBinNum = convertedFreq / (sampleRate / windowSize);

        // By default lrintf will round to nearest by default, rounding to even on tie.
        // std::round that was used here before rounds halfway cases away from zero.
        // However, we can probably tolerate rounding issues here, as this will only slightly affect
        // the visuals.
        return static_cast<int>(lrintf(convertedFreqBinNum));
    };

    unsigned char* const data = image.bits();

    for (int xx = 0; xx < paintableClipRect.width(); ++xx) {

        // zoomInfo must be queried for each column since with fisheye enabled
        // time between columns is variable
        const auto w0 = sampleCount(
            0.5 + sampleRate / stretchRatio
            * (zoomInfo.PositionToTime(xx, -leftOffset) - playStartTime));

        const auto w1 = sampleCount(
            0.5 + sampleRate / stretchRatio
            * (zoomInfo.PositionToTime(xx + 1, -leftOffset) - playStartTime));

        bool maybeSelected = ssel0 <= w0 && w1 < ssel1;
        maybeSelected = maybeSelected || (xx == selectedX);

        for (int yy = 0; yy < paintableClipRect.height(); ++yy) {
            const float bin     = bins[yy];
            const float nextBin = bins[yy + 1];

            // For spectral selection, determine what colour
            // set to use.  We use a darker selection if
            // in both spectral range and time range.
            AColor::ColorGradientChoice selected = AColor::ColorGradientUnselected;

            // If we are in the time selected range, then we may use a different color set.
            if (maybeSelected) {
                selected = ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                                          (xx + leftOffset - leftOffset) / DASH_LENGTH, isSpectral);
            }

            const float value = specPxCache->values[xx * paintableClipRect.height() + yy];
            unsigned char rv, gv, bv;
            GetColorGradient(value, selected, colorScheme, &rv, &gv, &bv);
            image.setPixelColor(xx, paintableClipRect.height() - yy - 1, QColor(rv, gv, bv));
        }
    }

    const QRectF targetRect(gc.metrics.left, gc.metrics.top, gc.metrics.width, gc.metrics.height);
    const QRectF sourceRect(0, 0, paintableClipRect.width(), paintableClipRect.height());
    painter.drawImage(targetRect, image, sourceRect);
}
}
