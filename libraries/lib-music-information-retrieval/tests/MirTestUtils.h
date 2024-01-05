#pragma once

#include "MirAudioReader.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <fstream>
#include <numeric>
#include <string>
#include <vector>

namespace MIR
{

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
template <typename Result>
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

   const auto numPositives = count_if(results.begin(), results.end(), truth);
   const auto numNegatives = results.size() - numPositives;

   // Find true and false positive rates for various score thresholds.
   // True positive and false positive counts are nondecreasing with i,
   // therefore if false positive rate has increased at some i, true positive
   // rate has not decreased.
   std::vector<double> truePositiveRates(results.size());
   std::vector<double> falsePositiveRates(results.size());
   for (size_t i = 0; i < results.size(); ++i)
   {
      const auto numTruePositives = count_if(
         results.begin(), results.begin() + i + 1, truth);
      const auto numFalsePositives = std::count_if(
         results.begin(), results.begin() + i + 1, falsity);
      truePositiveRates[i] =
         static_cast<double>(numTruePositives) / numPositives;
      falsePositiveRates[i] =
         static_cast<double>(numFalsePositives) / numNegatives;
   }

   // Now find the area under the non-decreasing curve with FPR as x-axis,
   // TPR as y, and i as a parameter.  (This curve is within a square with unit
   // side.)
   double auc = 0.;
   for (size_t i = 0; i <= results.size(); ++i)
   {
      const auto leftFpr = i == 0 ? 0. : falsePositiveRates[i - 1];
      const auto rightFpr = i == results.size() ? 1. : falsePositiveRates[i];
      const auto leftTpr = i == 0 ? 0. : truePositiveRates[i - 1];
      const auto rightTpr = i == results.size() ? 1. : truePositiveRates[i];
      const auto trapezoid = (rightTpr + leftTpr) * (rightFpr - leftFpr) / 2.;
      assert(trapezoid >= 0); // See comments above
      auc += trapezoid;
   }

   // Find the parameter at which the x coordinate exceeds the allowed FPR.
   const auto it = std::upper_bound(
      falsePositiveRates.begin(), falsePositiveRates.end(),
      allowedFalsePositiveRate);

   if (it == falsePositiveRates.end())
      // All breakpoints satify the constraint. Return the least score.
      return { auc, results.back().score };
   else if (it == falsePositiveRates.begin())
      // No breakpoint satisfies the constraint. Return the greatest score.
      return { auc, results.front().score };

   // For threshold, use the score halfway between the last breakpoint that
   // satisfies the constraint and the first breakpoint that doesn't.
   const auto index = it - falsePositiveRates.begin();
   const auto threshold = (results[index - 1].score + results[index].score) / 2;

   return { auc, threshold };
}

std::vector<std::string> GetWavFilesUnderDir(const char* dir);

void ProgressBar(int width, int percent);

template <typename T>
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

OctaveError GetOctaveError(double expected, double actual);

// A simple hashing that add the sum of all samples to the given hash.
// Reproducible benchmarking must use the same input. We use this to make sure
// that it does.
template <int bufferSize = 1024>
void UpdateHash(const MirAudioReader& source, float& hash)
{
   // Sum samples to hash.
   long long start = 0;
   std::array<float, bufferSize> buffer;
   while (true)
   {
      const auto numSamples =
         std::min<long long>(bufferSize, source.GetNumSamples() - start);
      if (numSamples == 0)
         break;
      source.ReadFloats(buffer.data(), start, numSamples);
      hash += std::accumulate(buffer.begin(), buffer.begin() + numSamples, 0.f);
      start += numSamples;
   }
}

} // namespace MIR
