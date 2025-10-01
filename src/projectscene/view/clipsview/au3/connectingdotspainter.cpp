#include "connectingdotspainter.h"

#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "samplespainterutils.h"
#include "WaveClip.h"
#include "WaveClipUtilities.h"
#include "WaveformSettings.h"
#include "WaveformScale.h"
#include "PendingTracks.h"
#include "ZoomInfo.h"

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
    Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(globalContext()->currentProject()->au3ProjectPtr());

    Au3WaveTrack* origWaveTrack = DomAccessor::findWaveTrack(*au3Project, Au3TrackId(clipKey.trackId));

    //! Pending tracks are same as project tracks, but with new tracks when recording, so we need draw them
    Au3Track* pendingTrack = &PendingTracks::Get(*au3Project)
                             .SubstitutePendingChangedTrack(*DomAccessor::findWaveTrack(*au3Project, Au3TrackId(clipKey.trackId)));

    Au3WaveTrack* track = dynamic_cast<Au3WaveTrack*>(pendingTrack);
    IF_ASSERT_FAILED(track) {
        return;
    }

    auto pendingClipId = DomAccessor::findMatchedClip(track, origWaveTrack, clipKey.clipId);
    if (pendingClipId == -1) {
        return;
    }

    std::shared_ptr<Au3WaveClip> waveClip = DomAccessor::findWaveClip(track, pendingClipId);
    IF_ASSERT_FAILED(waveClip) {
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
        // Draw center line at the middle of the current channel
        const int centerY = waveMetrics.top + waveMetrics.height / 2;
        samplespainterutils::drawCenterLine(painter, waveMetrics, params.style, centerY);
        const auto samples = samplespainterutils::getSampleData(*waveClip, index, waveMetrics, dB, dBRange, zoomMax, zoomMin);
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
