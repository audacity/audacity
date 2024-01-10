#include "MirAudioReaders.h"
#include "MirTestUtils.h"
#include "MusicInformationRetrieval.h"
#include "WavMirAudioReader.h"

#include <catch2/catch.hpp>
#include <fstream>
#include <iomanip>
#include <iostream>

namespace MIR
{
TEST_CASE("GetRocInfo")
{
   // We use the AUC as a measure of the classifier's performance. With a
   // suitable data set, this helps us detect regressions, and guide fine-tuning
   // of the algorithm. This test should help understand how it works and also
   // make sure that we've implemented that metric correctly :)

   struct Sample
   {
      bool truth;
      double score;
   };

   using Samples = std::vector<Sample>;

   struct Expected
   {
      double areaUnderCurve;
      double threshold;
   };

   struct TestCase
   {
      const Samples samples;
      const double allowedFalsePositiveRate;
      const Expected expected;
   };

   const std::vector<TestCase> testCases {
      // Classifier is upside down. We don't tolerate false positives. The
      // returned threshold is then 100 will satisfy this, but the TPR will also
      // be 0 ...
      TestCase { Samples { { true, 100. }, { false, 200. } }, 0.,
                 Expected { 0., 200. } },

      // Classifier is still upside down. We'll get true positives only if we
      // accept an FPR of 1.
      TestCase { Samples { { true, 100. }, { false, 200. } }, 1.,
                 Expected { 0., 100. } },

      // Now we have a classifier that works. We don't accept false positives.
      TestCase { Samples { { false, 100. }, { false, 150. }, { true, 200. } },
                 0., Expected { 1., 175. } },

      // A random classifier, which should have an AUC of 0.75.
      TestCase {
         Samples { { false, 1. }, { true, 2. }, { false, 3. }, { true, 4. } },
         0.5, Expected { .75, 1.5 } },
   };

   for (const auto& testCase : testCases)
   {
      const auto roc =
         GetRocInfo(testCase.samples, testCase.allowedFalsePositiveRate);
      REQUIRE(roc.areaUnderCurve == testCase.expected.areaUnderCurve);
      REQUIRE(roc.threshold == testCase.expected.threshold);
   }
}

TEST_CASE("GetChecksum")
{
   constexpr auto bufferSize = 5;
   const auto checksum = GetChecksum<bufferSize>(SquareWaveMirAudioReader {});
   REQUIRE(checksum == 0.);
}

auto ToString(const std::optional<TimeSignature>& ts)
{
   if (ts.has_value())
      switch (*ts)
      {
      case TimeSignature::TwoTwo:
         return std::string("2/2");
      case TimeSignature::FourFour:

         return std::string("4/4");
      case TimeSignature::ThreeFour:
         return std::string("3/4");
      case TimeSignature::SixEight:
         return std::string("6/8");
      default:
         return std::string("none");
      }
   else
      return std::string("none");
}

TEST_CASE("TatumQuantizationFitBenchmarking")
{
   if (!runLocally)
      return;

   // Running this test will update
   // `TatumQuantizationFitBenchmarkingOutput/summary.txt`. The summary contains
   //
   // 1. the AUC metric for regression-testing,
   // 2. the strict- and lenient-mode thresholds,
   // 3. the octave-error RMS (Schreiber, H., et al. (2020)), and
   // 4. the hash of the audio files used.
   //
   // The AUC can only be used for comparison if the hash doesn't change. At the
   // time of writing, the benchmarking can only conveniently be run on the
   // author's machine (Windows), because the files used are not
   // redistributable. Setting up a redistributable dataset that can be used by
   // other developers is currently being worked on.

   // We only observe the results for the most lenient classifier. The other,
   // stricter classifier will yield the same results, only with fewer false
   // positives.
   constexpr auto tolerance = FalsePositiveTolerance::Lenient;
   constexpr int progressBarWidth = 50;
   const auto wavFiles =
      GetWavFilesUnderDir("C:/Users/saint/Documents/auto-tempo");
   std::ofstream sampleValueCsv {
      std::string(CMAKE_CURRENT_SOURCE_DIR) +
      "/TatumQuantizationFitBenchmarkingOutput/sampleValues.csv"
   };
   sampleValueCsv
      << "truth,score,tatumRate,bpm,ts,octaveFactor,octaveError,lag,filename\n";

   float checksum = 0.f;
   struct Sample
   {
      bool truth;
      double score;
      std::optional<OctaveError> octaveError;
   };
   std::vector<Sample> samples;
   const auto numFiles = wavFiles.size();
   auto count = 0;
   std::transform(
      wavFiles.begin(), wavFiles.begin() + numFiles,
      std::back_inserter(samples), [&](const std::string& wavFile) {
         const WavMirAudioReader audio { wavFile };
         checksum += GetChecksum(audio);
         QuantizationFitDebugOutput debugOutput;
         std::function<void(double)> progressCb;
         GetMusicalMeterFromSignal(audio, tolerance, progressCb, &debugOutput);
         ProgressBar(progressBarWidth, 100 * count++ / numFiles);
         const auto expected = GetBpmFromFilename(wavFile);
         const auto truth = expected.has_value();
         const std::optional<OctaveError> error =
            truth ?
               std::make_optional(GetOctaveError(*expected, debugOutput.bpm)) :
               std::nullopt;
         sampleValueCsv << (truth ? "true" : "false") << ","
                        << debugOutput.score << ","
                        << 60. * debugOutput.tatumQuantization.numDivisions /
                              debugOutput.audioFileDuration
                        << "," << debugOutput.bpm << ","
                        << ToString(debugOutput.timeSignature) << ","
                        << (error.has_value() ? error->factor : 0.) << ","
                        << (error.has_value() ? error->remainder : 0.) << ","
                        << debugOutput.tatumQuantization.lag << "," << wavFile
                        << "\n";
         return Sample { truth, debugOutput.score, error };
      });

   // AUC of ROC curve. Tells how good our loop/not-loop clasifier is.
   const auto rocInfo = GetRocInfo(
      samples, loopClassifierSettings.at(tolerance).allowedFalsePositiveRate);

   const auto strictThreshold =
      GetRocInfo(
         samples, loopClassifierSettings.at(FalsePositiveTolerance::Strict)
                     .allowedFalsePositiveRate)
         .threshold;

   // Get RMS of octave errors. Tells how good the BPM estimation is.
   const auto octaveErrors = std::accumulate(
      samples.begin(), samples.end(), std::vector<double> {},
      [&](std::vector<double> octaveErrors, const Sample& sample) {
         if (sample.octaveError.has_value())
            octaveErrors.push_back(sample.octaveError->remainder);
         return octaveErrors;
      });
   const auto octaveErrorStd = std::sqrt(
      std::accumulate(
         octaveErrors.begin(), octaveErrors.end(), 0.,
         [&](double sum, double octaveError) {
            return sum + octaveError * octaveError;
         }) /
      octaveErrors.size());

   std::ofstream summaryFile {
      std::string(CMAKE_CURRENT_SOURCE_DIR) +
      "/TatumQuantizationFitBenchmarkingOutput/summary.txt"
   };

   summaryFile << std::setprecision(std::numeric_limits<double>::digits10 + 1)
               << "AUC: " << rocInfo.areaUnderCurve << "\n"
               << "Strict Threshold (Minutes-and-Seconds): " << strictThreshold
               << "\n"
               << "Lenient Threshold (Beats-and-Measures): "
               << rocInfo.threshold << "\n"
               << "Octave error RMS: " << octaveErrorStd << "\n"
               << "Audio file checksum: " << checksum << "\n";

   constexpr auto previousAuc = 0.9121725731895223;

   // If this changed, then some non-refactoring code change happened. If
   // `rocInfo.areaUnderCurve > previousAuc`, then there's probably no argument
   // about the change. On the contrary, though, the change is either an
   // inadvertent bug, and if it is deliberate, should be well justified.
   REQUIRE(rocInfo.areaUnderCurve == previousAuc);
}
} // namespace MIR
