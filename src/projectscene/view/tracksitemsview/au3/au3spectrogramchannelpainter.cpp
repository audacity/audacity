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

void Au3SpectrogramChannelPainter::paint(QPainter& painter, WaveClipChannel& clipChannel, const WaveMetrics& metrics, const Params& params)
{
    SpectrogramSettings& settings = params.settings;
    Au3SelectedRegion selectedRegion;
    selectedRegion.setT0(params.selectedRegion.t0);
    selectedRegion.setT1(params.selectedRegion.t1);
    selectedRegion.setF0(params.selectedRegion.f0);
    selectedRegion.setF1(params.selectedRegion.f1);

    const QRect rect{ static_cast<int>(metrics.left), 0, static_cast<int>(metrics.width), static_cast<int>(metrics.height) };
    const ClipParameters clipParams { clipChannel, rect, params.zoomInfo };

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

    const float* spectrogram = nullptr;
    const sampleCount* where = nullptr;
    WaveClipSpectrumCache::Get(clipChannel).GetSpectrogram(clipChannel, spectrogram, settings, where,
                                                           hiddenMid.width(), t0, averagePixelsPerSecond);
}
}
