/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramutils.h"
#include "ispectrogramconfiguration.h"

#include <cmath>

int au::spectrogram::viewportWidth(const ViewInfo& viewInfo)
{
    return static_cast<int>(std::round((viewInfo.viewportEndTime - viewInfo.viewportStartTime) * viewInfo.pixelsPerSecond));
}

double au::spectrogram::positionToTime(const ViewInfo& viewInfo, int position)
{
    return viewInfo.viewportStartTime + position / viewInfo.pixelsPerSecond;
}

long long au::spectrogram::timeToPosition(const ViewInfo& viewInfo, double projectTime)
{
    double t = 0.5 + viewInfo.pixelsPerSecond * (projectTime - viewInfo.viewportStartTime);
    if (t < INT64_MIN) {
        return INT64_MIN;
    }
    if (t > INT64_MAX) {
        return INT64_MAX;
    }
    t = std::floor(t);
    return t;
}

void au::spectrogram::findCorrection(
    const std::vector<long long>& oldWhere, size_t oldLen, size_t newLen,
    double t0, double sampleRate, double stretchRatio, double samplesPerPixel,
    int& oldX0, double& correction)
{
    // Mitigate the accumulation of location errors
    // in copies of copies of ... of caches.
    // Look at the loop that populates "where" below to understand this.

    // Find the sample position that is the origin in the old cache.
    const double oldWhere0 = static_cast<double>(oldWhere[1]) - samplesPerPixel;
    const double oldWhereLast = oldWhere0 + oldLen * samplesPerPixel;
    // Find the length in samples of the old cache.
    const double denom = oldWhereLast - oldWhere0;

    // What sample would go in where[0] with no correction?
    const double guessWhere0 = t0 * sampleRate / stretchRatio;

    if ( // Skip if old and NEW are disjoint:
        oldWhereLast <= guessWhere0
        || guessWhere0 + newLen * samplesPerPixel <= oldWhere0
        ||// Skip unless denom rounds off to at least 1.
        denom < 0.5) {
        // The computation of oldX0 in the other branch
        // may underflow and the assertion would be violated.
        oldX0 = oldLen;
        correction = 0.0;
    } else {
        // What integer position in the old cache array does that map to?
        // (even if it is out of bounds)
        oldX0 = floor(0.5 + oldLen * (guessWhere0 - oldWhere0) / denom);
        // What sample count would the old cache have put there?
        const double where0 = oldWhere0 + double(oldX0) * samplesPerPixel;
        // What correction is needed to align the NEW cache with the old?
        const double correction0 = where0 - guessWhere0;
        correction = std::clamp(correction0, -samplesPerPixel, samplesPerPixel);
        assert(correction == correction0);
    }
}

void au::spectrogram::fillWhere(
    std::vector<long long>& where, size_t len, bool addBias, double correction,
    double t0, double sampleRate, double stretchRatio, double samplesPerPixel)
{
    // Be careful to make the first value non-negative
    const auto bias = addBias ? .5 : 0.;
    const double w0 = 0.5 + correction + bias + t0 * sampleRate / stretchRatio;
    where[0] = std::max(0.0, floor(w0));
    for (decltype(len) x = 1; x < len + 1; x++) {
        where[x] = floor(w0 + double(x) * samplesPerPixel);
    }
}

int au::spectrogram::fftLength(const ISpectrogramConfiguration& config)
{
    return (1 << config.winSizeLog2()) * ((config.algorithm() != SpectrogramAlgorithm::Pitch) ? config.zeroPaddingFactor() : 1);
}

std::pair<float, float> au::spectrogram::spectrogramBounds(const ISpectrogramConfiguration& config, double sampleRate)
{
    const auto type = config.scale();

    auto min = 0.f;
    auto max = 0.f;

    auto bottom = 0.f;
    if (type == SpectrogramScale::Period) {
        // special case
        const auto half = fftLength(config) / 2;
        // EAC returns no data for below this frequency:
        const float bin2 = sampleRate / half;
        bottom = bin2;
    } else if (type != SpectrogramScale::Linear) {
        // logarithmic, etc.
        bottom = 1.0f;
    }

    const float top = sampleRate / 2;
    if (config.maxFreq() < 0) {
        max = top;
    } else {
        max = std::clamp<double>(config.maxFreq(), bottom, top);
    }

    auto spectrumMin = config.minFreq();
    if (spectrumMin < 0) {
        min = std::max(bottom, top / 1000.0f);
    } else {
        min = std::clamp<double>(spectrumMin, bottom, top);
    }

    return std::make_pair(min, max);
}
