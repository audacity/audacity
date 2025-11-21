#include "connectingdotspainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "samplespainterutils.h"
#include "WaveClip.h"
#include "PendingTracks.h"

using namespace au::au3;
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
    if (!track) {
        return;
    }

    std::shared_ptr<WaveClip> waveClip = au::au3::DomAccessor::findWaveClip(track, clipKey.itemId);
    if (!waveClip) {
        return;
    }

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    const float dBRange = std::abs(params.dbRange);
    const bool dB = !params.isLinear;
    const double trimLeft = waveClip->GetTrimLeft();

    auto waveMetrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        waveMetrics.height = channelHeight[index];
        samplespainterutils::drawBackground(painter, waveMetrics, params.style, trimLeft);
        // Draw center line at the middle of the current channel
        const int centerY = waveMetrics.top + waveMetrics.height / 2;
        samplespainterutils::drawCenterLine(painter, waveMetrics, params.style, centerY);
        const auto samples = samplespainterutils::getSampleData(*waveClip, index, waveMetrics, dB, dBRange, params.verticalZoom,
                                                                -params.verticalZoom);
        if (samples.size() == 0) {
            continue;
        }
        if (params.showClipping) {
            samplespainterutils::drawClippedSamples(samples, waveMetrics, painter, params.style);
        }

        drawConnectingPoints(samples, waveMetrics, painter, params.style);
        waveMetrics.top += waveMetrics.height;
    }
}
}
