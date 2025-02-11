#include "connectingdotspainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "samplespainterutils.h"
#include "WaveClip.h"
#include "WaveClipUtilities.h"
#include "WaveformSettings.h"
#include "WaveformScale.h"
#include "ZoomInfo.h"

namespace {
void drawConnectingPoints(const au::projectscene::SampleData& samples, const au::projectscene::WaveMetrics& metrics, QPainter& painter,
                          const au::projectscene::IWavePainter::Style& style)
{
    painter.setPen(style.samplePen);

    const size_t slen = samples.size();
    for (size_t s = 0; s < slen - 1; s++) {
        painter.drawLine(
            metrics.left + samples.x[s], metrics.top + samples.y[s],
            metrics.left + samples.x[s + 1], metrics.top + samples.y[s + 1]);
    }
}
}

namespace au::projectscene {
void ConnectingDotsPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
{
    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    IF_ASSERT_FAILED(track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.clipId);
    if (!waveClip) {
        return;
    }

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    float zoomMin, zoomMax;
    const auto& cache = WaveformScale::Get(*track);
    cache.GetDisplayBounds(zoomMin, zoomMax);

    const auto& settings = WaveformSettings::Get(*track);
    const float dBRange = settings.dBRange;
    const bool dB = !settings.isLinear();
    const double trimLeft = waveClip->GetTrimLeft();

    auto waveMetrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        waveMetrics.height = channelHeight[index];
        samplespainterutils::drawBackground(painter, waveMetrics, params.style, trimLeft);
        samplespainterutils::drawBaseLine(painter, waveMetrics, params.style);
        const auto samples = samplespainterutils::getSampleData(*waveClip, index, waveMetrics, dB, dBRange, zoomMax, zoomMin);
        if (samples.size() == 0) {
            continue;
        }
        drawConnectingPoints(samples, waveMetrics, painter, params.style);
        waveMetrics.top += waveMetrics.height;
    }
}
}
