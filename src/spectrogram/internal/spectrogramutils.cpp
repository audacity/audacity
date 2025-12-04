/*
 * Audacity: A Digital Audio Editor
 */
#include "spectrogramutils.h"

#include <cmath>

int au::spectrogram::viewportWidth(const ViewInfo& viewInfo)
{
    return static_cast<int>(std::round((viewInfo.viewportT1 - viewInfo.viewportT0) * viewInfo.pixelsPerSecond));
}

double au::spectrogram::positionToTime(const ViewInfo& viewInfo, int position)
{
    return viewInfo.viewportT0 + position / viewInfo.pixelsPerSecond;
}

int au::spectrogram::timeToPosition(const ViewInfo& viewInfo, double projectTime)
{
    double t = 0.5 + viewInfo.pixelsPerSecond * (projectTime - viewInfo.viewportT0);
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
