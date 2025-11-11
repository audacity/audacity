/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramchannelpainter.h"

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

Au3SpectrogramChannelPainter::Params::Params(SpectrogramSettings& settings,
                                             const SelectedRegion& selectedRegion,
                                             const ZoomInfo& zoomInfo,
                                             bool trackIsSelected)
    : settings{settings}
    , zoomInfo{zoomInfo}
    , selectedRegion{selectedRegion}
    , trackIsSelected{trackIsSelected}
{
}

Au3SpectrogramChannelPainter::Au3SpectrogramChannelPainter(std::weak_ptr<au3::Au3Project> au3Project)
    : m_au3Project{std::move(au3Project)}
{
}

void Au3SpectrogramChannelPainter::paint(QPainter& painter, WaveClipChannel& clipChannel, const WaveChannel& trackChannel,
                                         const WaveMetrics& metrics, const Params& params)
{
    SpectrogramSettings& settings = params.settings;
    const ZoomInfo& zoomInfo = params.zoomInfo;
    Au3SelectedRegion selectedRegion;
    selectedRegion.setT0(params.selectedRegion.t0);
    selectedRegion.setT1(params.selectedRegion.t1);
    selectedRegion.setF0(params.selectedRegion.f0);
    selectedRegion.setF1(params.selectedRegion.f1);

    const QRect rect{ static_cast<int>(metrics.left), 0, static_cast<int>(metrics.width), static_cast<int>(metrics.height) };
    const ClipParameters clipParams { clipChannel, rect, zoomInfo };

    const QRect& hiddenMid = clipParams.hiddenMid;
    // The "hiddenMid" rect contains the part of the display actually
    // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
    if (hiddenMid.width() <= 0) {
        return;
    }

    const double& t0 = clipParams.t0;
    const double playStartTime = clipChannel.GetPlayStartTime();

    const auto [ssel0, ssel1] = GetSelectedSampleIndices(selectedRegion, clipChannel, params.trackIsSelected);
    const double& averagePixelsPerSecond = clipParams.averagePixelsPerSecond;
    const double sampleRate = clipChannel.GetRate();
    const double stretchRatio = clipChannel.GetStretchRatio();
    const double& hiddenLeftOffset = clipParams.hiddenLeftOffset;
    const double& leftOffset = clipParams.leftOffset;
    const QRect& mid = clipParams.mid;

    double freqLo = SelectedRegion::UndefinedFrequency;
    double freqHi = SelectedRegion::UndefinedFrequency;
    freqLo = selectedRegion.f0();
    freqHi = selectedRegion.f1();

    const int& colorScheme = settings.colorScheme;
    const int& range = settings.range;
    const int& gain = settings.gain;

    QImage image(mid.width(), mid.height(), QImage::Format_RGB888);

    const float* spectrogram = nullptr;
    const sampleCount* where = nullptr;
    const auto half = settings.GetFFTLength() / 2;
    const double binUnit = sampleRate / (2 * half);
    const bool updated = WaveClipSpectrumCache::Get(clipChannel).GetSpectrogram(clipChannel, spectrogram, settings, where,
                                                                                hiddenMid.width(), t0, averagePixelsPerSecond);

    auto nBins = settings.NBins();

    float minFreq, maxFreq;
    SpectrogramBounds::Get(trackChannel).GetBounds(trackChannel, minFreq, maxFreq);

    const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

    // nearest frequency to each pixel row from number scale, for selecting
    // the desired fft bin(s) for display on that row
    float* bins = (float*)alloca(sizeof(*bins) * (hiddenMid.height() + 1));
    {
        const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));

        NumberScale::Iterator it = numberScale.begin(mid.height());
        float nextBin = std::max(0.0f, std::min(float(nBins - 1),
                                                settings.findBin(*it, binUnit)));

        int yy;
        for (yy = 0; yy < hiddenMid.height(); ++yy) {
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
        && ((int)specPxCache->len == hiddenMid.height() * hiddenMid.width())
        && scaleType == specPxCache->scaleType
        && gain == specPxCache->gain
        && range == specPxCache->range
        && minFreq == specPxCache->minFreq
        && maxFreq == specPxCache->maxFreq
        ) {
        // Wave clip's spectrum cache is up to date,
        // and so is the spectrum pixel cache
    } else {
        // Update the spectrum pixel cache
        specPxCache = std::make_unique<SpecPxCache>(hiddenMid.width() * hiddenMid.height());
        specPxCache->scaleType = scaleType;
        specPxCache->gain = gain;
        specPxCache->range = range;
        specPxCache->minFreq = minFreq;
        specPxCache->maxFreq = maxFreq;

        for (int xx = 0; xx < hiddenMid.width(); ++xx) {
            for (int yy = 0; yy < hiddenMid.height(); ++yy) {
                const float bin     = bins[yy];
                const float nextBin = bins[yy + 1];
                const float value = findValue(spectrogram + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
                specPxCache->values[xx * hiddenMid.height() + yy] = value;
            } // each yy
        } // each xx
    } // updating cache

    float selBinLo = settings.findBin(freqLo, binUnit);
    float selBinHi = settings.findBin(freqHi, binUnit);
    float selBinCenter = (freqLo < 0 || freqHi < 0)
                         ? -1
                         : settings.findBin(sqrt(freqLo * freqHi), binUnit);

    const bool isSpectral = settings.SpectralSelectionEnabled();
    constexpr bool hidden = true; // Used to check if fisheye is enabled
    constexpr int begin = 0;
    constexpr int end = 0;
    constexpr size_t numPixels = std::max(0, end - begin);

    SpecCache specCache;

    // need explicit resize since specCache.where[] accessed before Populate()
    specCache.Grow(numPixels, settings, -1, t0);

    if (numPixels > 0) {
        for (int ii = begin; ii < end; ++ii) {
            const double time = zoomInfo.PositionToTime(ii, -leftOffset) - playStartTime;
            specCache.where[ii - begin]
                =sampleCount(0.5 + sampleRate / stretchRatio * time);
        }
        specCache.Populate(
            settings, clipChannel, 0, 0, numPixels,
            0 // FIXME: PRL -- make reassignment work with fisheye
            );
    }

    // build color gradient tables (not thread safe)
    if (!AColor::gradient_inited) {
        AColor::PreComputeGradient();
    }

    // Bug 2389 - always draw at least one pixel of selection.
    int selectedX = zoomInfo.TimeToPosition(selectedRegion.t0(), -leftOffset);

    const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));
    const int windowSize = settings.WindowSize();

    // Lambda for converting yy (not mouse coord!) to respective freq. bins
    auto yyToFreqBin = [&](int yy){
        const double p = double(yy) / hiddenMid.height();
        float convertedFreq = numberScale.PositionToValue(p);
        float convertedFreqBinNum = convertedFreq / (sampleRate / windowSize);

        // By default lrintf will round to nearest by default, rounding to even on tie.
        // std::round that was used here before rounds halfway cases away from zero.
        // However, we can probably tolerate rounding issues here, as this will only slightly affect
        // the visuals.
        return static_cast<int>(lrintf(convertedFreqBinNum));
    };

    unsigned char* const data = image.bits();

    for (int xx = 0; xx < mid.width(); ++xx) {
        int correctedX = xx + leftOffset - hiddenLeftOffset;

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

        // In case the xx matches the hop number, it will be used as iterator for frequency bins
        std::set<int>* pSelectedBins = nullptr;
        std::set<int>::iterator freqBinIter;
        auto advanceFreqBinIter = [&](int nextBinRounded){
            while (freqBinIter != pSelectedBins->end()
                   && *freqBinIter < nextBinRounded) {
                ++freqBinIter;
            }
        };

        for (int yy = 0; yy < hiddenMid.height(); ++yy) {
            const float bin     = bins[yy];
            const float nextBin = bins[yy + 1];
            auto binRounded = yyToFreqBin(yy);
            auto nextBinRounded = yyToFreqBin(yy + 1);

            // For spectral selection, determine what colour
            // set to use.  We use a darker selection if
            // in both spectral range and time range.

            AColor::ColorGradientChoice selected = AColor::ColorGradientUnselected;

            // If we are in the time selected range, then we may use a different color set.
            if (maybeSelected) {
                selected = ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                                          (xx + leftOffset - hiddenLeftOffset) / DASH_LENGTH, isSpectral);
            }

            const float value = specPxCache->values[correctedX * hiddenMid.height() + yy];

            unsigned char rv, gv, bv;
            GetColorGradient(value, selected, colorScheme, &rv, &gv, &bv);
            int px = ((mid.height() - 1 - yy) * mid.width() + xx);
            px *=3;
            data[px++] = rv;
            data[px++] = gv;
            data[px] = bv;
        } // each yy
    } // each xx

    const QRectF targetRect(metrics.left, metrics.top, metrics.width, metrics.height);
    const QRectF sourceRect(0, 0, mid.width(), mid.height());
    painter.drawImage(targetRect, image, sourceRect);
}
}
