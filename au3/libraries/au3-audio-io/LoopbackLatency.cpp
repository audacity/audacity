/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file LoopbackLatency.cpp

 **********************************************************************/

#include "LoopbackLatency.h"

#include <algorithm>
#include <cmath>
#include <cstdint>

namespace LoopbackLatency {
std::vector<float> MakeBurst(float gain)
{
    std::vector<float> burst;
    burst.reserve(BurstLength);
    uint32_t seed = 0x1234567u;
    for (size_t i = 0; i < BurstLength; ++i) {
        seed = seed * 1664525u + 1013904223u;
        burst.push_back((seed >> 31) ? gain : -gain);
    }
    return burst;
}

Hit FindBurst(const std::vector<float>& capture, const std::vector<float>& burst, size_t begin, size_t span)
{
    Hit hit;
    if (span == 0 || begin + span + burst.size() > capture.size()) {
        return hit;
    }
    double best = 0.0;
    double sumSquares = 0.0;
    for (size_t lag = 0; lag < span; ++lag) {
        double c = 0.0;
        const float* x = capture.data() + begin + lag;
        for (size_t j = 0; j < burst.size(); ++j) {
            c += burst[j] * x[j];
        }
        sumSquares += c * c;
        if (std::fabs(c) > std::fabs(best)) {
            best = c;
            hit.lag = static_cast<long>(lag);
        }
    }
    const double rms = std::sqrt(sumSquares / span);
    hit.confidence = rms > 0 ? std::fabs(best) / rms : 0.0;
    hit.inverted = best < 0;
    return hit;
}

Analysis Analyse(const std::vector<float>& capture, const std::vector<float>& burst, size_t periodFrames)
{
    Analysis result;
    for (const float v : capture) {
        result.peak = std::max(result.peak, std::fabs(v));
    }
    if (periodFrames <= burst.size()) {
        return result;
    }

    std::vector<long> lags;
    size_t invertedCount = 0;
    result.minConfidence = 0.0;
    const size_t span = periodFrames - burst.size();
    for (size_t begin = 0; begin + periodFrames <= capture.size(); begin += periodFrames) {
        ++result.burstsTotal;
        const Hit hit = FindBurst(capture, burst, begin, span);
        if (hit.lag < 0 || hit.confidence < MinConfidence) {
            continue;
        }
        result.minConfidence = lags.empty() ? hit.confidence : std::min(result.minConfidence, hit.confidence);
        lags.push_back(hit.lag);
        invertedCount += hit.inverted ? 1 : 0;
    }

    result.burstsFound = lags.size();
    if (lags.empty()) {
        return result;
    }
    std::sort(lags.begin(), lags.end());
    result.roundTripFrames = lags[lags.size() / 2];
    result.spreadFrames = lags.back() - lags.front();
    result.inverted = invertedCount * 2 > lags.size();
    return result;
}
}
