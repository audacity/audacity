#include "minmaxrmspainter.h"

#include <algorithm>
#include <cmath>
#include <cstring>

#include "au3-clip-processing/ClipFades.h"
#include "au3-mixer/Envelope.h"
#include "au3-utility/MemoryX.h"
#include "au3-wave-track-paint/GraphicsDataCache.h"
#include "au3wrap/internal/domaccessor.h"
#include "wavepainterutils.h"
#include "WaveformPainter.h"
#include "../wavedisplaybounds.h"

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

constexpr uint64_t fingerprintSeed = 14695981039346656037ull;

void mixIntoFingerprint(uint64_t& hash, double value)
{
    uint64_t bits = 0;
    std::memcpy(&bits, &value, sizeof(bits));
    hash = (hash ^ bits) * 1099511628211ull;
    hash ^= hash >> 29;
}

//! Changes whenever the envelope's effect on the waveform may have changed.
//! Envelope::GetVersion() is not bumped by every mutation (SetOffset,
//! InsertSpace, EnvPoint::SetVal...), so what GetValues() reads is hashed instead.
uint64_t envelopeFingerprint(const Envelope& envelope)
{
    uint64_t hash = fingerprintSeed;
    mixIntoFingerprint(hash, envelope.GetDefaultValue());
    mixIntoFingerprint(hash, envelope.GetOffset());
    mixIntoFingerprint(hash, envelope.GetTrackLen());
    mixIntoFingerprint(hash, envelope.GetExponential() ? 1.0 : 0.0);

    const size_t count = envelope.GetNumberOfPoints();
    mixIntoFingerprint(hash, static_cast<double>(count));
    for (size_t i = 0; i < count; ++i) {
        const EnvPoint& point = envelope[static_cast<int>(i)];
        mixIntoFingerprint(hash, point.GetT());
        mixIntoFingerprint(hash, point.GetVal());
    }

    return hash;
}

uint64_t fadesFingerprint(const ClipFades::Snapshot& fades)
{
    uint64_t hash = fingerprintSeed;
    mixIntoFingerprint(hash, fades.playDuration);
    mixIntoFingerprint(hash, fades.fadeIn);
    mixIntoFingerprint(hash, fades.fadeOut);
    mixIntoFingerprint(hash, static_cast<double>(fades.fadeInType));
    mixIntoFingerprint(hash, static_cast<double>(fades.fadeOutType));
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
    mixIntoFingerprint(data.dataVersion, data.samplesPerSecond);
    const Envelope& envelope = waveClip->GetEnvelope();
    if (envelope.GetNumberOfPoints() > 0 || envelope.GetDefaultValue() != 1.0) {
        data.dataVersion = (data.dataVersion * 1099511628211ull) ^ envelopeFingerprint(envelope);
    }
    const auto fades = ClipFades::Get(*waveClip).snapshot();
    if (fades.fadeIn > 0 || fades.fadeOut > 0) {
        data.dataVersion = (data.dataVersion * 1099511628211ull) ^ fadesFingerprint(fades);
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

    const auto [displayMin, displayMax] = waveDisplayBounds(params);
    WavePaintParameters paintParams;
    paintParams
    .SetDisplayParameters(metrics.height, displayMin, displayMax, params.showClipping)
    .SetDBParameters(std::abs(params.dbRange), !params.isLinear)
    .SetShowRMS(params.showRMS);
    paintParams.SetEnvelope(waveClip->GetEnvelope());

    bool complete = true;
    WaveformPainter::Get(*waveClip).GetColumnData(channelIndex, paintParams, metrics, columns, &complete);
    return complete;
}

void MinMaxRMSPainter::samples(const trackedit::ClipKey& clipKey, size_t channelIndex, const IWavePainter::Params& params,
                               const WaveMetrics& metrics, std::vector<WaveSamplePoint>& samples)
{
    samples.clear();

    const std::shared_ptr<WaveClip> waveClip = findClip(clipKey);
    if (!waveClip || channelIndex >= waveClip->NChannels() || metrics.zoom <= 0.0 || metrics.height <= 0.0) {
        return;
    }

    const double rate = waveClip->GetRate() / waveClip->GetStretchRatio();
    const double trimLeft = waveClip->GetTrimLeft();
    const sampleCount trimSamples = waveClip->TimeToSamples(trimLeft);
    const sampleCount visibleSamples = waveClip->GetVisibleSampleCount();

    // Sample indices from the untrimmed sequence start
    const sampleCount first = std::max(trimSamples, sampleCount(std::floor(metrics.fromTime * rate)) - 1);
    const sampleCount last = std::min(trimSamples + visibleSamples - 1, sampleCount(std::ceil(metrics.toTime * rate)) + 1);
    if (last < first) {
        return;
    }
    const size_t count = (last - first + 1).as_size_t();

    //! NOTE: All callers run on the GUI thread; sharing the buffer keeps its capacity
    static std::vector<float> buffer;
    buffer.resize(count);
    waveClip->GetSamples(channelIndex, reinterpret_cast<samplePtr>(buffer.data()), floatSample, first - trimSamples, count, false);

    const Envelope& envelope = waveClip->GetEnvelope();
    const auto fades = ClipFades::Get(*waveClip).snapshot();
    const bool dB = !params.isLinear;
    const float dBRange = std::abs(params.dbRange);
    const auto [displayMin, displayMax] = waveDisplayBounds(params);
    const auto height = static_cast<float>(metrics.height);
    const bool hasSelection = metrics.selectionEndTime > metrics.selectionStartTime;

    samples.reserve(count);
    for (size_t i = 0; i < count; ++i) {
        const double time = (first + i).as_double() / rate;
        const double gain = envelope.GetValue(time + envelope.GetOffset(), 1.0 / rate) * fades.gainAt(time - trimLeft);
        float value = static_cast<float>(buffer[i] * gain);

        WaveSamplePoint point;
        point.x = static_cast<float>(metrics.left + (time - metrics.fromTime) * metrics.zoom);
        point.clipping = value <= -MAX_AUDIO || value >= MAX_AUDIO;
        point.selected = hasSelection && time >= metrics.selectionStartTime && time <= metrics.selectionEndTime;

        if (dB) {
            value = getDBValue(value, dBRange);
        }
        const float y = displayMax == displayMin ? height * 0.5f
                        : (displayMax - value) / (displayMax - displayMin) * (height - 1.0f);
        point.y = std::clamp(y, -1.0f, height);

        samples.push_back(point);
    }
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
