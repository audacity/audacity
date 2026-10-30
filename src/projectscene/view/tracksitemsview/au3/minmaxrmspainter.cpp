#include "minmaxrmspainter.h"

#include <cstring>

#include "au3-mixer/Envelope.h"
#include "au3-wave-track-paint/GraphicsDataCache.h"
#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "WaveformPainter.h"

using namespace au::au3;

namespace {
graphics::Color ColorFromQColor(const QColor& color)
{
    return graphics::Color(color.red(), color.green(), color.blue(), color.alpha());
}

float getDBValue(float value, float dbRange)
{
    float sign = (value >= 0 ? 1 : -1);

    if (value != 0.) {
        float db = LINEAR_TO_DB(fabs(value));
        value = (db + dbRange) / dbRange;

        if (value < 0.0) {
            value = 0.0;
        }

        value *= sign;
    }

    return value;
}

//! Changes whenever the envelope's effect on the waveform may have changed.
//! Envelope::GetVersion() is not bumped by every mutation (SetOffset,
//! InsertSpace, EnvPoint::SetVal...), so what GetValues() reads is hashed instead.
uint64_t envelopeFingerprint(const Envelope& envelope)
{
    uint64_t hash = 14695981039346656037ull;
    auto mix = [&hash](double value) {
        uint64_t bits = 0;
        std::memcpy(&bits, &value, sizeof(bits));
        hash = (hash ^ bits) * 1099511628211ull;
        hash ^= hash >> 29;
    };

    mix(envelope.GetDefaultValue());
    mix(envelope.GetOffset());
    mix(envelope.GetTrackLen());
    mix(envelope.GetExponential() ? 1.0 : 0.0);

    const size_t count = envelope.GetNumberOfPoints();
    mix(static_cast<double>(count));
    for (size_t i = 0; i < count; ++i) {
        const EnvPoint& point = envelope[static_cast<int>(i)];
        mix(point.GetT());
        mix(point.GetVal());
    }

    return hash;
}
}

namespace au::projectscene {
std::shared_ptr<au::au3::Au3WaveClip> MinMaxRMSPainter::findClip(const trackedit::ClipKey& clipKey) const
{
    const auto project = globalContext()->currentProject();
    if (!project) {
        return nullptr;
    }

    au::au3::Au3Project* au3Project = reinterpret_cast<au::au3::Au3Project*>(project->au3ProjectPtr());
    WaveTrack* track = au::au3::DomAccessor::findWaveTrack(*au3Project, TrackId(clipKey.trackId));
    return track ? au::au3::DomAccessor::findWaveClip(track, clipKey.itemId) : nullptr;
}

IMinMaxRMSPainter::ClipData MinMaxRMSPainter::clipData(const trackedit::ClipKey& clipKey)
{
    const std::shared_ptr<WaveClip> waveClip = findClip(clipKey);
    if (!waveClip) {
        return {};
    }

    ClipData data;
    data.valid = true;
    data.channelCount = waveClip->NChannels();
    data.playStartTime = waveClip->GetPlayStartTime();
    data.playEndTime = waveClip->GetPlayEndTime();
    data.trimLeft = waveClip->GetTrimLeft();
    data.sequenceEndTime = waveClip->GetSequenceEndTime();
    data.samplesPerSecond = waveClip->GetRate() / waveClip->GetStretchRatio();
    data.tileWidth = GraphicsDataCacheBase::CacheElementWidth;

    data.dataVersion = WaveformPainter::Get(*waveClip).Generation();
    const Envelope& envelope = waveClip->GetEnvelope();
    if (envelope.GetNumberOfPoints() > 0 || envelope.GetDefaultValue() != 1.0) {
        data.dataVersion = (data.dataVersion * 1099511628211ull) ^ envelopeFingerprint(envelope);
    }

    return data;
}

bool MinMaxRMSPainter::columns(const trackedit::ClipKey& clipKey, size_t channelIndex, const IWavePainter::Params& params,
                               const WaveMetrics& metrics, std::vector<WaveColumnVertex>& columns)
{
    const std::shared_ptr<WaveClip> waveClip = findClip(clipKey);
    if (!waveClip) {
        columns.clear();
        return true;
    }

    WavePaintParameters paintParams;
    paintParams
    .SetDisplayParameters(metrics.height, params.displayBounds.first, params.displayBounds.second, params.showClipping)
    .SetDBParameters(std::abs(params.dbRange), !params.isLinear)
    .SetShowRMS(params.showRMS);
    paintParams.SetEnvelope(waveClip->GetEnvelope());

    bool complete = true;
    WaveformPainter::Get(*waveClip).GetColumnData(channelIndex, paintParams, metrics, columns, &complete);
    return complete;
}

void MinMaxRMSPainter::paint(QPainter& painter, const trackedit::ClipKey& clipKey, const IWavePainter::Params& params)
{
    const std::shared_ptr<WaveClip> waveClip = findClip(clipKey);
    if (!waveClip) {
        return;
    }

    const float dbRange = std::abs(params.dbRange);
    const bool dB = !params.isLinear;

    const std::vector<double> channelHeight {
        params.geometry.height * params.channelHeightRatio,
        params.geometry.height * (1 - params.channelHeightRatio),
    };

    const float zoomMin = dB ? getDBValue(params.displayBounds.first, dbRange) : params.displayBounds.first;
    const float zoomMax = dB ? getDBValue(params.displayBounds.second, dbRange) : params.displayBounds.second;

    auto& waveformPainter = WaveformPainter::Get(*waveClip);
    WavePaintParameters paintParameters;

    auto metrics = wavepainterutils::getWaveMetrics(globalContext()->currentProject(), clipKey, params);

    for (size_t index = 0; index < waveClip->NChannels(); index++) {
        metrics.height = channelHeight[index];
        paintParameters
        .SetDisplayParameters(
            metrics.height, zoomMin, zoomMax, params.showClipping)
        .SetDBParameters(dbRange, dB)
        .SetBlankColor(ColorFromQColor(params.style.blankBrush))
        .SetSampleColors(
            ColorFromQColor(params.style.samplePen),
            ColorFromQColor(params.style.selectedSamplePen))
        .SetShowRMS(params.showRMS)
        .SetRMSColors(
            ColorFromQColor(params.style.rmsPen),
            ColorFromQColor(params.style.rmsSelectedPen))
        .SetBackgroundColors(
            ColorFromQColor(params.style.normalBackground),
            ColorFromQColor(params.style.selectedBackground))
        .SetClippingColors(
            ColorFromQColor(params.style.clippedPen),
            ColorFromQColor(params.style.clippedPen))
        .SetEnvelopeColors(
            ColorFromQColor(params.style.envelopeBackground),
            ColorFromQColor(params.style.selectedEnvelopeBackground))
        .SetEnvelope(waveClip->GetEnvelope());

        au::projectscene::WaveMetrics _metrics = metrics;
        _metrics.fromTime += waveClip->GetTrimLeft();
        _metrics.toTime += waveClip->GetTrimLeft();

        metrics.top += static_cast<int>(metrics.height);

        waveformPainter.Draw(index, painter, paintParameters, _metrics);
    }
}
}
