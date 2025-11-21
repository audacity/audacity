/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

Audacity: A Digital Audio Editor

GetMeterUsingTatumQuantizationFit.cpp

Matthieu Hodgkinson

**********************************************************************/

#include "GetMeterUsingTatumQuantizationFit.h"
#include "IteratorX.h"
#include "MapToPositiveHalfIndex.h"
#include "MirDsp.h"
#include "MirTypes.h"
#include "MirUtils.h"
#include "MusicInformationRetrieval.h"
#include <array>
#include <cassert>
#include <cmath>
#include <map>
#include <numeric>
#include <regex>
#include <unordered_map>
#include <unordered_set>

namespace MIR {
namespace {
constexpr auto minTatumsPerMinute = 100;
constexpr auto maxTatumsPerMinute = 700;
constexpr auto minBpm = 50.;
constexpr auto maxBpm = 200.;
constexpr auto minBeatsPerBar = 2;
constexpr auto maxBeatsPerBar = 4;
constexpr std::array<std::pair<int, int>, 9> possibleTatumsPerBeat {
    std::pair<int, int> { 1, 1 }, //
    std::pair<int, int> { 2, 1 }, std::pair<int, int> { 3, 1 },
    std::pair<int, int> { 4, 1 }, std::pair<int, int> { 6, 1 }, //
    std::pair<int, int> { 1, 2 }, std::pair<int, int> { 1, 3 },
    std::pair<int, int> { 1, 4 }, std::pair<int, int> { 1, 6 } //
};

// Number of bars and beats per bar dividing the input audio recording.
struct BarDivision
{
    const int numBars;
    const int beatsPerBar;
};

// A map of possible number of tatums to a list of number of bars and beats per
// bar which explain it.
using PossibleDivHierarchies
    =std::unordered_map<int /*num tatums*/, std::vector<BarDivision> >;

// Gather a collection of possible numbers of divisions based on the assumption
// that the audio is a loop (hence there must be a round number of bars) and on
// reasonable bar and tatum durations.
PossibleDivHierarchies GetPossibleDivHierarchies(double audioFileDuration)
{
    constexpr auto minBarDuration = 1.;
    constexpr auto maxBarDuration = 4.;
    const int minNumBars
        =std::max(std::round(audioFileDuration / maxBarDuration), 1.);
    const int maxNumBars = std::round(audioFileDuration / minBarDuration);
    PossibleDivHierarchies possibleDivHierarchies;
    for_each_in_range(
        IotaRange { minNumBars, maxNumBars + 1 }, [&](int numBars) {
        const auto barDuration = audioFileDuration / numBars;
        const auto minBpb = std::clamp<int>(
            std::floor(minBpm * barDuration / 60), minBeatsPerBar,
            maxBeatsPerBar);
        const auto maxBpb = std::clamp<int>(
            std::ceil(maxBpm * barDuration / 60), minBeatsPerBar,
            maxBeatsPerBar);
        for_each_in_range(
            IotaRange { minBpb, maxBpb + 1 }, [&](int beatsPerBar) {
            std::for_each(
                possibleTatumsPerBeat.begin(), possibleTatumsPerBeat.end(),
                [&](const std::pair<int, int>& tatumsPerBeat) {
                const auto [tatumsPerBeatNum, tatumsPerBeatDen]
                    =tatumsPerBeat;

                // Number of tatums per bar must be round:
                if (
                    (beatsPerBar * tatumsPerBeatNum) % tatumsPerBeatDen
                    != 0) {
                    return;
                }

                const int tatumsPerBar
                    =beatsPerBar * tatumsPerBeatNum / tatumsPerBeatDen;
                const int numTatums = tatumsPerBar * numBars;
                const auto tatumRate = 60. * numTatums / audioFileDuration;
                if (
                    minTatumsPerMinute < tatumRate
                    && tatumRate < maxTatumsPerMinute) {
                    possibleDivHierarchies[numTatums].push_back(
                        BarDivision { numBars, beatsPerBar });
                }
            });
        });
    });
    return possibleDivHierarchies;
}

int GetOnsetLag(const std::vector<float>& odf, int numTatums)
{
    // Understandably, the first onset of a loop recording isn't typically
    // centered on time 0, or there would be a click at the onset. The entire
    // recording is therefore likely to have a small lag. It is important that we
    // take this lag into account for the quantization distance measure.
    // The code below is equivalent to cross-correlating the odf with a pulse
    // train of frequency `numTatums / odf.size()`. We take the position of the
    // first peak to be the lag.
    const auto pulseTrainPeriod = 1. * odf.size() / numTatums;
    auto max = std::numeric_limits<float>::lowest();
    auto lag = 0;
    while (true)
    {
        auto val = 0.f;
        for_each_in_range(IotaRange { 0, numTatums }, [&](int i) {
            const int j = std::round(i * pulseTrainPeriod) + lag;
            val += (j < odf.size() ? odf[j] : 0.f);
        });
        if (val < max) {
            break;
        }
        max = val;
        ++lag;
    }
    return lag - 1;
}

// This is the fundament of the algorithm. It gives a weighted average of the
// normalized distance between ODF peaks and the closest tatum. The weights are
// the ODF values at the peaks. The distance is normalized by the tatum
// duration, so that the final value ranges between 0 and 1.
// This can be seen as a performance accuracy measure. The higher the tempo, the
// more accurate the performer has to be for this distance to stay low. In
// general, this is desired. We could nevertheless consider introducing a slight
// tolerance which increases with the tatum rate, as very fast rhythmic patterns
// recorded without post-editing still tend to achieve lower scores.
double GetQuantizationDistance(
    const std::vector<int>& peakIndices, const std::vector<float>& peakValues,
    size_t size, int numDivisions, int lag)
{
    std::vector<double> peakDistances(peakIndices.size());
    const auto peakSum
        =std::accumulate(peakValues.begin(), peakValues.end(), 0.);
    const auto odfSamplesPerDiv = 1. * size / numDivisions;
    std::transform(
        peakIndices.begin(), peakIndices.end(), peakDistances.begin(),
        [&](int peakIndex) {
        const auto shiftedIndex = peakIndex - lag;
        const auto closestDiv = std::round(shiftedIndex / odfSamplesPerDiv);
        // Normalized distance between 0 and 1:
        const auto distance
            =(shiftedIndex - closestDiv * odfSamplesPerDiv) / odfSamplesPerDiv;
        // Mutliply by two such that the error spans `[0, 1)`.
        return 2 * std::abs(distance);
    });
    // Calculate the score as the sum of the distances weighted by
    // the odf values:
    const auto weightedAverage
        =std::inner_product(
              peakDistances.begin(), peakDistances.end(), peakValues.begin(), 0.)
          / peakSum;
    return weightedAverage;
}

OnsetQuantization RunQuantizationExperiment(
    const std::vector<float>& odf, const std::vector<int>& peakIndices,
    const std::vector<float>& peakValues,
    const std::vector<int>& possibleNumTatums,
    QuantizationFitDebugOutput* debugOutput)
{
    const auto quantizations = [&]() {
        std::unordered_map<int, OnsetQuantization> quantizations;
        std::transform(
            possibleNumTatums.begin(), possibleNumTatums.end(),
            std::inserter(quantizations, quantizations.end()), [&](int numTatums) {
            const auto lag = GetOnsetLag(odf, numTatums);
            const auto distance = GetQuantizationDistance(
                peakIndices, peakValues, odf.size(), numTatums, lag);
            return std::make_pair(
                numTatums, OnsetQuantization { distance, lag, numTatums });
        });
        return quantizations;
    }();

    const auto bestFitIt = std::min_element(
        quantizations.begin(), quantizations.end(),
        [](const std::pair<int, OnsetQuantization>& a,
           const std::pair<int, OnsetQuantization>& b) {
        return a.second.error < b.second.error;
    });

    const auto error = bestFitIt->second.error;
    const auto mostLikelyNumTatums = bestFitIt->first;
    const auto lag = bestFitIt->second.lag;

    return { error, lag, mostLikelyNumTatums };
}

std::optional<TimeSignature>
GetTimeSignature(const BarDivision& barDivision, int numTatums)
{
    const auto numBeats = barDivision.numBars * barDivision.beatsPerBar;
    const auto tatumsPerBeat = 1. * numTatums / numBeats;
    switch (barDivision.beatsPerBar) {
    case 2:
        return (tatumsPerBeat == 3) ? TimeSignature::SixEight
               : TimeSignature::TwoTwo;
    case 3:
        return TimeSignature::ThreeFour;
    case 4:
        return TimeSignature::FourFour;
    default:
        // Since the bar-division hypotheses are based on these very time
        // signatures, this should never happen. But the code would need to be
        // structured more clearly to really have confidence and risk crashing.
        // For now we accept that the time-signature cannot be recognized in
        // release builds.
        assert(false);
        return std::nullopt;
    }
}

double
GetTimeSignatureLikelihood(const std::optional<TimeSignature>& ts, double bpm)
{
    // TODO these were taken from a rather old PoC - review.
    if (!ts.has_value()) {
        return 0.;
    }

    static const std::unordered_map<TimeSignature, double> expectedBpms {
        { TimeSignature::TwoTwo, 115. },
        { TimeSignature::FourFour, 115. },
        { TimeSignature::ThreeFour, 140. },
        { TimeSignature::SixEight, 64. },
    };

    static const std::unordered_map<TimeSignature, double> bpmStdDevs {
        { TimeSignature::TwoTwo, 25. },
        { TimeSignature::FourFour, 25. },
        { TimeSignature::ThreeFour, 25. },
        { TimeSignature::SixEight, 15. },
    };

    // Add a slight bias towards 4/4, which is the most common time signature.
    static const std::unordered_map<TimeSignature, double> tsPrior {
        { TimeSignature::TwoTwo, .1 },
        { TimeSignature::FourFour, .45 },
        { TimeSignature::ThreeFour, .2 },
        { TimeSignature::SixEight, .25 },
    };

    const auto mu = expectedBpms.at(*ts);
    const auto sigma = bpmStdDevs.at(*ts);
    const auto tmp = (bpm - mu) / sigma;
    const auto likelihood = std::exp(-.5 * tmp * tmp);
    return likelihood * tsPrior.at(*ts);
}

// To evaluate how likely a certain BPM is, we evaluate how much the ODF repeats
// itself at that beat rate. We do this by looking at the auto-correlation of
// the ODF, "comb-filtering" it at integer multiples of the beat period.
double GetBeatSelfSimilarityScore(
    double odfAutoCorrSampleRate, double bpm,
    const std::vector<float>& odfAutoCorr, int odfAutocorrFullSize,
    QuantizationFitDebugOutput* debugOutput)
{
    // Look for closest peak in `odfAutoCorr`:
    const auto lag = odfAutoCorrSampleRate * 60 / bpm;
    // Traverse all the auto-correlation by steps of `k*lag`, each time
    // finding the closest peak, and average the values. Doing this rather
    // than evaluating only just one peak is more robust to rhythms which
    // miss beats.
    auto periodIndex = 1;
    auto sum = 0.;
    auto numIndices = 0;
    while (true)
    {
        auto j = static_cast<int>(periodIndex++ *lag + .5);
        if (j >= odfAutoCorr.size()) {
            break;
        }
        while (true)
        {
            const auto i = MapToPositiveHalfIndex(j - 1, odfAutocorrFullSize);
            const auto k = MapToPositiveHalfIndex(j + 1, odfAutocorrFullSize);
            if (
                odfAutoCorr[i] <= odfAutoCorr[j]
                && odfAutoCorr[j] >= odfAutoCorr[k]) {
                break;
            }
            j = odfAutoCorr[i] > odfAutoCorr[k] ? i : k;
        }
        sum += odfAutoCorr[j];
        ++numIndices;
        if (debugOutput) {
            debugOutput->odfAutoCorrPeakIndices.push_back(j);
        }
    }
    return sum / numIndices;
}

size_t GetBestBarDivisionIndex(
    const std::vector<BarDivision>& possibleBarDivisions,
    double audioFileDuration, int numTatums, const std::vector<float>& odf,
    QuantizationFitDebugOutput* debugOutput)
{
    const auto odfAutoCorr = GetNormalizedCircularAutocorr(odf);
    if (debugOutput) {
        debugOutput->odfAutoCorr = odfAutoCorr;
    }
    const auto odfAutocorrFullSize = 2 * (odfAutoCorr.size() - 1);
    assert(IsPowOfTwo(odfAutocorrFullSize));
    const auto odfAutoCorrSampleRate = odfAutocorrFullSize / audioFileDuration;

    std::vector<double> scores(possibleBarDivisions.size());
    // These (still) only depend on the beat rate. We look at
    // self-similarities at beat intervals, so to say, without examining the
    // contents of the beats. Since several bar divisions may yield the same
    // number of beats, we may be able to re-use some calculations.
    std::unordered_map<int /*numBeats*/, double> autocorrScoreCache;
    std::transform(
        possibleBarDivisions.begin(), possibleBarDivisions.end(), scores.begin(),
        [&](const BarDivision& barDivision) {
        const auto numBeats = barDivision.numBars * barDivision.beatsPerBar;
        const auto timeSignature = GetTimeSignature(barDivision, numTatums);
        const auto bpm = 1. * numBeats / audioFileDuration * 60;
        const auto likelihood = GetTimeSignatureLikelihood(timeSignature, bpm);
        if (!autocorrScoreCache.count(numBeats)) {
            autocorrScoreCache[numBeats] = GetBeatSelfSimilarityScore(
                odfAutoCorrSampleRate, bpm, odfAutoCorr, odfAutocorrFullSize,
                debugOutput);
        }
        const auto selfSimilarityScore = autocorrScoreCache.at(numBeats);
        return likelihood * selfSimilarityScore;
    });

    return std::max_element(scores.begin(), scores.end()) - scores.begin();
}

MusicalMeter GetMostLikelyMeterFromQuantizationExperiment(
    const std::vector<float>& odf, int numTatums,
    std::vector<BarDivision> possibleBarDivisions, double audioFileDuration,
    QuantizationFitDebugOutput* debugOutput)
{
    std::vector<BarDivision> fourFourDivs;
    for_each_in_range(
        IotaRange<size_t>(0, possibleBarDivisions.size()), [&](size_t i) {
        if (
            GetTimeSignature(possibleBarDivisions[i], numTatums)
            == TimeSignature::FourFour) {
            fourFourDivs.push_back(possibleBarDivisions[i]);
        }
    });

    // If there is one or more 4/4 possibilities, don't take any risk and only
    // consider these because much more frequent, especially in the world of
    // loops. When we get more clever about time-signature detection, we may be
    // more assertive.
    if (!fourFourDivs.empty()) {
        std::swap(possibleBarDivisions, fourFourDivs);
    }

    const auto winnerIndex = GetBestBarDivisionIndex(
        possibleBarDivisions, audioFileDuration, numTatums, odf, debugOutput);

    const auto& barDivision = possibleBarDivisions[winnerIndex];
    const auto numBeats = barDivision.numBars * barDivision.beatsPerBar;
    const auto signature = GetTimeSignature(barDivision, numTatums);
    const auto bpm = 60. * numBeats / audioFileDuration;

    return { bpm, signature };
}

bool IsSingleEvent(
    const std::vector<int>& peakIndices, const std::vector<float>& peakValues)
{
    // Detect single-event recordings, e.g. only just one crash cymbal hit. This
    // will translate as one large peak and maybe a few much smaller ones. In
    // that case we can expect the average to be between these two groups.
    const auto peakAvg
        =std::accumulate(peakValues.begin(), peakValues.end(), 0.)
          / peakIndices.size();
    const auto numPeaksAboveAvg
        =std::count_if(peakValues.begin(), peakValues.end(), [&](float v) {
        return v > peakAvg;
    });
    return numPeaksAboveAvg <= 1;
}
} // namespace

std::optional<MusicalMeter> GetMeterUsingTatumQuantizationFit(
    const MirAudioReader& audio, FalsePositiveTolerance tolerance,
    const std::function<void(double)>& progressCallback,
    QuantizationFitDebugOutput* debugOutput)
{
    const auto odf
        =GetOnsetDetectionFunction(audio, progressCallback, debugOutput);
    const auto odfSr
        =1. * audio.GetSampleRate() * odf.size() / audio.GetNumSamples();
    const auto audioFileDuration
        =1. * audio.GetNumSamples() / audio.GetSampleRate();

    const auto peakIndices = GetPeakIndices(odf);
    if (debugOutput) {
        debugOutput->audioFileDuration = audioFileDuration;
        debugOutput->odfSr = odfSr;
        debugOutput->odfPeakIndices = peakIndices;
    }

    const auto peakValues = ([&]() {
        std::vector<float> peakValues(peakIndices.size());
        std::transform(
            peakIndices.begin(), peakIndices.end(), peakValues.begin(),
            [&](int i) { return odf[i]; });
        return peakValues;
    })();

    if (IsSingleEvent(peakIndices, peakValues)) {
        return {}
    }

    const auto possibleDivs = GetPossibleDivHierarchies(audioFileDuration);
    if (possibleDivs.empty()) {
        // The file is probably too short to be a loop.
        return {}
    }

    const auto possibleNumTatums = [&]() {
        std::vector<int> possibleNumTatums(possibleDivs.size());
        std::transform(
            possibleDivs.begin(), possibleDivs.end(), possibleNumTatums.begin(),
            [&](const auto& entry) { return entry.first; });
        return possibleNumTatums;
    }();

    const auto experiment = RunQuantizationExperiment(
        odf, peakIndices, peakValues, possibleNumTatums, debugOutput);

    const auto winnerMeter = GetMostLikelyMeterFromQuantizationExperiment(
        odf, experiment.numDivisions, possibleDivs.at(experiment.numDivisions),
        audioFileDuration, debugOutput);

    const auto score = 1 - experiment.error;

    if (debugOutput) {
        debugOutput->tatumQuantization = experiment;
        debugOutput->bpm = winnerMeter.bpm;
        debugOutput->timeSignature = winnerMeter.timeSignature;
        debugOutput->odf = odf;
        debugOutput->odfSr = odfSr;
        debugOutput->audioFileDuration = audioFileDuration;
        debugOutput->score = score;
    }

    return score < loopClassifierSettings.at(tolerance).threshold
           ? std::optional<MusicalMeter> {}
           : winnerMeter;
}
} // namespace MIR
