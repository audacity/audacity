/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramclipchannelpainter.h"

#include "internal/spectrogramutils.h"

#include "./ClipParameters.h"
#include "./SpectrumCache.h"

#include "spectrogram/spectrogramcolors.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"
#include "libraries/lib-screen-geometry/NumberScale.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-wave-track-settings/SpectrogramSettings.h"

namespace au::spectrogram {
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

inline SpectrogramColors::ColorGradientChoice
ChooseColorSet(float bin0, float bin1, float selBinLo,
               float selBinCenter, float selBinHi, int dashCount, bool isSpectral)
{
    if (!isSpectral) {
        return SpectrogramColors::ColorGradientTimeSelected;
    }
    if ((selBinCenter >= 0) && (bin0 <= selBinCenter)
        && (selBinCenter < bin1)) {
        return SpectrogramColors::ColorGradientEdge;
    }
    if ((0 == dashCount % 2)
        && (((selBinLo >= 0) && (bin0 <= selBinLo) && (selBinLo < bin1))
            || ((selBinHi >= 0) && (bin0 <= selBinHi) && (selBinHi < bin1)))) {
        return SpectrogramColors::ColorGradientEdge;
    }
    if ((selBinLo < 0 || selBinLo < bin1) && (selBinHi < 0 || selBinHi > bin0)) {
        return SpectrogramColors::ColorGradientTimeAndFrequencySelected;
    }

    return SpectrogramColors::ColorGradientTimeSelected;
}
}

Au3SpectrogramClipChannelPainter::Au3SpectrogramClipChannelPainter(std::shared_ptr<WaveClipChannel> channel)
    : m_waveClipChannel{std::move(channel)} {}

void Au3SpectrogramClipChannelPainter::paint(QImage& image,
                                             const SpectrogramGlobalContext& gc,
                                             const SpectrogramTrackContext& tc)
{
    SpectrogramSettings& settings = tc.settings;
    const ZoomInfo& zoomInfo = gc.zoomInfo;
    auto& clipChannel = *m_waveClipChannel;
    Au3SelectedRegion selectedRegion;
    // Careful: t1 must be set before t0
    selectedRegion.setT1(gc.selectedRegion.t1);
    selectedRegion.setT0(gc.selectedRegion.t0);
    selectedRegion.setF0(gc.selectedRegion.f0);
    selectedRegion.setF1(gc.selectedRegion.f1);

    const QRect paintableRect{ 0, 0, zoomInfo.viewportWidth(), image.height() };
    const ClipParameters clipParams { clipChannel, paintableRect, zoomInfo }; // TODO try to get rid of ClipParameters

    const int imageWidth = image.width();
    const int imageHeight = image.height();

    const double& visibleT0 = clipParams.visibleT0;
    const double playStartTime = clipChannel.GetPlayStartTime();

    const auto [ssel0, ssel1] = GetSelectedSampleIndices(selectedRegion, clipChannel, tc.trackIsSelected);
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

    const float* spectrogram = nullptr;
    const sampleCount* where = nullptr;
    const auto half = settings.GetFFTLength() / 2;
    const double binUnit = sampleRate / (2 * half);
    const bool updated = WaveClipSpectrumCache::Get(clipChannel).GetSpectrogram(clipChannel, spectrogram, settings, where, imageWidth,
                                                                                visibleT0, zoomInfo.zoom);

    auto nBins = settings.NBins();

    const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

    // nearest frequency to each pixel row from number scale, for selecting
    // the desired fft bin(s) for display on that row
    float* bins = (float*)alloca(sizeof(*bins) * (imageHeight + 1));
    {
        const NumberScale numberScale(settings.GetScale(tc.minFreq, tc.maxFreq));

        NumberScale::Iterator it = numberScale.begin(imageHeight);
        float nextBin = std::max(0.0f, std::min(float(nBins - 1),
                                                settings.findBin(*it, binUnit)));

        int yy;
        for (yy = 0; yy < imageHeight; ++yy) {
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
        && ((int)specPxCache->len == imageHeight * imageWidth)
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
        specPxCache = std::make_unique<SpecPxCache>(imageWidth * imageHeight);
        specPxCache->scaleType = scaleType;
        specPxCache->gain = gain;
        specPxCache->range = range;
        specPxCache->minFreq = tc.minFreq;
        specPxCache->maxFreq = tc.maxFreq;

        for (int xx = 0; xx < imageWidth; ++xx) {
            for (int yy = 0; yy < imageHeight; ++yy) {
                const float bin     = bins[yy];
                const float nextBin = bins[yy + 1];
                const float value = findValue(spectrogram + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
                specPxCache->values[xx * imageHeight + yy] = value;
            }
        }
    } // updating cache

    float selBinLo = settings.findBin(freqLo, binUnit);
    float selBinHi = settings.findBin(freqHi, binUnit);
    float selBinCenter = (freqLo < 0 || freqHi < 0)
                         ? -1
                         : settings.findBin(sqrt(freqLo * freqHi), binUnit);

    const bool isSpectral = settings.SpectralSelectionEnabled();

    SpecCache specCache;

    // need explicit resize since specCache.where[] accessed before Populate()
    specCache.Grow(0, settings, -1, visibleT0);

    // build color gradient tables (not thread safe)
    if (!SpectrogramColors::gradient_inited) {
        SpectrogramColors::PreComputeGradient();
    }

    // Bug 2389 - always draw at least one pixel of selection.
    int selectedX = zoomInfo.timeToPosition(selectedRegion.t0()) - leftOffset;

    for (int xx = 0; xx < imageWidth; ++xx) {
        // zoomInfo must be queried for each column since with fisheye enabled
        // time between columns is variable
        const auto w0 = sampleCount(
            0.5 + sampleRate / stretchRatio
            * (zoomInfo.positionToTime(xx - leftOffset) - playStartTime));

        const auto w1 = sampleCount(
            0.5 + sampleRate / stretchRatio
            * (zoomInfo.positionToTime(xx + 1 - leftOffset) - playStartTime));
        bool maybeSelected = ssel0 <= w0 && w1 < ssel1;
        maybeSelected = maybeSelected || (xx == selectedX);

        for (int yy = 0; yy < imageHeight; ++yy) {
            const float bin     = bins[yy];
            const float nextBin = bins[yy + 1];

            // For spectral selection, determine what colour
            // set to use.  We use a darker selection if
            // in both spectral range and time range.
            SpectrogramColors::ColorGradientChoice selected = SpectrogramColors::ColorGradientUnselected;

            // If we are in the time selected range, then we may use a different color set.
            if (maybeSelected) {
                selected = ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                                          (xx + leftOffset - leftOffset) / DASH_LENGTH, isSpectral);
            }

            const float value = specPxCache->values[xx * imageHeight + yy];
            unsigned char rv, gv, bv;
            GetColorGradient(value, selected, colorScheme, &rv, &gv, &bv);
            image.setPixelColor(xx, imageHeight - yy - 1, QColor(rv, gv, bv));
        }
    }
}
}
