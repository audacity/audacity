/*
 * Audacity: A Digital Audio Editor
 */
#include "equalizationenvelopeutils.h"

#include <algorithm>
#include <cmath>
#include <vector>

#include "au3-builtin-effects/EqualizationFilter.h"
#include "au3-mixer/Envelope.h"

namespace au::effects::eq_common {
namespace {
constexpr double kLoFreqRef = 20.0;
}

void envLogToLin(EqualizationFilter& parameters)
{
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;
    const auto& hiFreq = parameters.mHiFreq;

    const size_t numPoints = logEnvelope.GetNumberOfPoints();
    if (numPoints == 0) {
        return;
    }

    std::vector<double> when(numPoints);
    std::vector<double> value(numPoints);

    linEnvelope.Flatten(0.0);
    linEnvelope.SetTrackLen(1.0);
    logEnvelope.GetPoints(when.data(), value.data(), static_cast<int>(numPoints));
    linEnvelope.Reassign(0.0, value[0]);

    const double loLog = std::log10(kLoFreqRef);
    const double hiLog = std::log10(hiFreq);
    const double denom = hiLog - loLog;

    for (size_t i = 0; i < numPoints; ++i) {
        linEnvelope.Insert(std::pow(10.0, when[i] * denom + loLog) / hiFreq, value[i]);
    }
    linEnvelope.Reassign(1.0, value[numPoints - 1]);
}

bool envLinToLog(EqualizationFilter& parameters)
{
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;
    const auto& hiFreq = parameters.mHiFreq;

    const size_t numPoints = linEnvelope.GetNumberOfPoints();
    if (numPoints == 0) {
        return false;
    }

    std::vector<double> when(numPoints);
    std::vector<double> value(numPoints);

    logEnvelope.Flatten(0.0);
    logEnvelope.SetTrackLen(1.0);
    linEnvelope.GetPoints(when.data(), value.data(), static_cast<int>(numPoints));
    logEnvelope.Reassign(0.0, value[0]);

    const double loLog = std::log10(kLoFreqRef);
    const double hiLog = std::log10(hiFreq);
    const double denom = hiLog - loLog;
    bool changed = false;

    for (size_t i = 0; i < numPoints; ++i) {
        if (when[i] * hiFreq >= kLoFreqRef) {
            // Caution: on Linux, when (when * hiFreq) == 20, the log
            // rounds to just under zero, which would assert.
            const double flog = (std::log10(when[i] * hiFreq) - loLog) / denom;
            logEnvelope.Insert(std::max(0.0, flog), value[i]);
        } else {
            changed = true;
            logEnvelope.Insert(0.0, value[i]);
        }
    }
    logEnvelope.Reassign(1.0, value[numPoints - 1]);
    return changed;
}
}
