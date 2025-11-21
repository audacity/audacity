/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MirTestUtils.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "MirTypes.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <fstream>
#include <functional>
#include <numeric>
#include <string>
#include <vector>

namespace MIR {
// Some tests, such as benchmarking and visualization, are not meant to be run
// on CI. This variable is used to disable them.
static constexpr auto runLocally = false;

struct RocInfo
{
    const double areaUnderCurve;
    const double threshold;
};

/*!
 * The Receiver Operating Characteristic (ROC) curve is a plot of the true
 * positive rate (TPR) against the false positive rate (FPR) for the different
 * possible thresholds of a binary classifier. The area under the curve (AUC)
 * is a measure of the classifier's performance. The greater the AUC, the
 * better the classifier.
 *
 * @tparam Result has public members `truth`, boolean, and `score`, numeric
 * @param results true classifications and scores of some population
 * @pre at least one of `results` is really positive (`truth` is true), and at
 * least one is really negative
 * @pre `0. <= allowedFalsePositiveRate && allowedFalsePositiveRate <= 1.`
 */
template<typename Result>
RocInfo
GetRocInfo(std::vector<Result> results, double allowedFalsePositiveRate = 0.)
{
    const auto truth = std::mem_fn(&Result::truth);
    const auto falsity = std::not_fn(truth);

    // There is at least one positive and one negative sample.
    assert(any_of(results.begin(), results.end(), truth));
    assert(any_of(results.begin(), results.end(), falsity));

    assert(allowedFalsePositiveRate >= 0. && allowedFalsePositiveRate <= 1.);
    allowedFalsePositiveRate = std::clamp(allowedFalsePositiveRate, 0., 1.);

    // Sort the results by score, descending.
    std::sort(results.begin(), results.end(), [](const auto& a, const auto& b) {
        return a.score > b.score;
    });

    const auto size = results.size();
    const auto numPositives = count_if(results.begin(), results.end(), truth);
    const auto numNegatives = size - numPositives;

    // Find true and false positive rates for various score thresholds.
    // True positive and false positive counts are nondecreasing with i,
    // therefore if false positive rate has increased at some i, true positive
    // rate has not decreased.
    std::vector<double> truePositiveRates;
    truePositiveRates.reserve(size);
    std::vector<double> falsePositiveRates;
    falsePositiveRates.reserve(size);
    size_t numTruePositives = 0;
    size_t numFalsePositives = 0;
    for (const auto& result : results) {
        if (result.truth) {
            ++numTruePositives;
        } else {
            ++numFalsePositives;
        }
        truePositiveRates.push_back(
            static_cast<double>(numTruePositives) / numPositives);
        falsePositiveRates.push_back(
            static_cast<double>(numFalsePositives) / numNegatives);
    }

    // Now find the area under the non-decreasing curve with FPR as x-axis,
    // TPR as y, and i as a parameter.  (This curve is within a square with unit
    // side.)
    double auc = 0.;
    for (size_t i = 0; i <= size; ++i) {
        const auto leftFpr = i == 0 ? 0. : falsePositiveRates[i - 1];
        const auto rightFpr = i == size ? 1. : falsePositiveRates[i];
        const auto leftTpr = i == 0 ? 0. : truePositiveRates[i - 1];
        const auto rightTpr = i == size ? 1. : truePositiveRates[i];
        const auto trapezoid = (rightTpr + leftTpr) * (rightFpr - leftFpr) / 2.;
        assert(trapezoid >= 0); // See comments above
        auc += trapezoid;
    }

    // Find the parameter at which the x coordinate exceeds the allowed FPR.
    const auto it = std::upper_bound(
        falsePositiveRates.begin(), falsePositiveRates.end(),
        allowedFalsePositiveRate);

    if (it == falsePositiveRates.end()) {
        // All breakpoints satify the constraint. Return the least score.
        return { auc, results.back().score }
    } else if (it == falsePositiveRates.begin()) {
        // No breakpoint satisfies the constraint. Return the greatest score.
        return { auc, results.front().score }
    }

    // For threshold, use the score halfway between the last breakpoint that
    // satisfies the constraint and the first breakpoint that doesn't.
    const auto index = it - falsePositiveRates.begin();
    const auto threshold = (results[index - 1].score + results[index].score) / 2;

    return { auc, threshold };
}

void ProgressBar(int width, int percent);

template<typename T>
void PrintPythonVector(
    std::ofstream& ofs, const std::vector<T>& v, const char* name)
{
    ofs << name << " = [";
    std::for_each(v.begin(), v.end(), [&](T x) { ofs << x << ","; });
    ofs << "]\n";
}

struct OctaveError
{
    double factor;
    double remainder;
};

/*!
 * @brief Gets the tempo detection octave error, as defined in section 5. of
 * Schreiber, H., Urbano, J. and Müller, M., 2020. Music Tempo Estimation: Are
 * We Done Yet?.  Transactions of the International Society for Music
 * Information Retrieval,  3(1), p.111–125. DOI:
 * https://doi.org/10.5334/tismir.43
 * In short, with an example: two bars of a fast 3/4 can in some cases be
 * interpreted as one bar of 6/8. However, there are 6 beats in the former,
 * against 2 in the latter, leading to an "octave error" of 3. In that case, the
 * returned `factor` would be 3, and the remainder, `log2(3 * actual /
 * expected)`
 */
OctaveError GetOctaveError(double expected, double actual);

// Reproducible benchmarking must use the same input. We use this to make sure
// that it does.
template<int bufferSize = 1024> float GetChecksum(const MirAudioReader& source)
{
    // Sum samples to checksum.
    float checksum = 0.f;
    long long start = 0;
    std::array<float, bufferSize> buffer;
    while (true)
    {
        const auto numSamples
            =std::min<long long>(bufferSize, source.GetNumSamples() - start);
        if (numSamples == 0) {
            break;
        }
        source.ReadFloats(buffer.data(), start, numSamples);
        checksum
            +=std::accumulate(buffer.begin(), buffer.begin() + numSamples, 0.f);
        start += numSamples;
    }
    return checksum;
}
} // namespace MIR
