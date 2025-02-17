#include "MirFakes.h"
#include "MirTestUtils.h"
#include "MusicInformationRetrieval.h"
#include "WavMirAudioReader.h"

#include <catch2/catch.hpp>
#include <chrono>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>

#define USE_FILESYSTEM (__has_include(< filesystem >) && _WIN32)

#if USE_FILESYSTEM
#   include <filesystem>
#endif

namespace MIR {
namespace {
const auto datasetRoot
    =std::string(CMAKE_CURRENT_SOURCE_DIR) + "/benchmarking-dataset";

std::vector<std::string> GetBenchmarkingAudioFiles()
{
    std::vector<std::string> files;
#if USE_FILESYSTEM
    namespace fs = std::filesystem;
    for (const auto& entry : fs::directory_iterator(datasetRoot)) {
        for (const auto& subEntry : fs::recursive_directory_iterator(entry)) {
            if (
                subEntry.is_regular_file() && subEntry.path().extension() == ".mp3") {
                files.push_back(subEntry.path().string());
            }
        }
    }
#else
    // Recursively find all files in the dataset directory with .mp3 extension,
    // not using std::filesystem:
    // https://stackoverflow.com/questions/612097/how-can-i-get-the-list-of-files-in-a-directory-using-c-or-c
    const auto command = "find -H " + datasetRoot + " -type f -name '*.mp3' -print";
    FILE* pipe = popen(command.c_str(), "r");
    if (!pipe) {
        throw std::runtime_error("popen() failed!");
    }
    constexpr auto bufferSize = 512;
    char buffer[bufferSize];
    while (fgets(buffer, bufferSize, pipe) != nullptr)
    {
        std::string file(buffer);
        file.erase(file.find_last_not_of("\n") + 1);
        files.push_back(file);
    }
    const auto returnCode = pclose(pipe);
    if (returnCode != 0) {
        throw std::runtime_error("pclose() failed!");
    }
#endif
    std::sort(files.begin(), files.end());
    return files;
}

std::string Pretty(const std::string& filename)
{
    // Remove the dataset root from the filename ...
    const auto datasetRootLength = datasetRoot.length();
    auto tmp = filename.substr(datasetRootLength + 1);
    // ... and now the .mp3 extension:
    tmp = tmp.substr(0, tmp.length() - 4);
    // Replace backslashes with forward slashes:
    std::replace(tmp.begin(), tmp.end(), '\\', '/');
    return tmp;
}
} // namespace

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

    for (const auto& testCase : testCases) {
        const auto roc
            =GetRocInfo(testCase.samples, testCase.allowedFalsePositiveRate);
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
    if (ts.has_value()) {
        switch (*ts) {
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
    } else {
        return std::string("none");
    }
}

TEST_CASE("TatumQuantizationFitBenchmarking")
{
    // For this test to run, you will need to set `runLocally` to `true`, and
    // you'll also need the benchmarking sound files. To get these, just open
    // `download-benchmarking-dataset.html` in a browser. This will download a
    // zip file that you'll need to extract and place in a `benchmarking-dataset`
    // directory under this directory.

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
    if (!runLocally) {
        return;
    }

    constexpr auto tolerance = FalsePositiveTolerance::Lenient;
    constexpr int progressBarWidth = 50;
    const auto audioFiles = GetBenchmarkingAudioFiles();
    std::stringstream sampleValueCsv;
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
    const auto numFiles = audioFiles.size();
    auto count = 0;
    std::chrono::milliseconds computationTime { 0 };
    std::transform(
        audioFiles.begin(), audioFiles.begin() + numFiles,
        std::back_inserter(samples), [&](const std::string& wavFile) {
        const WavMirAudioReader audio { wavFile };
        checksum += GetChecksum(audio);
        QuantizationFitDebugOutput debugOutput;
        std::function<void(double)> progressCb;
        const auto now = std::chrono::steady_clock::now();
        GetMusicalMeterFromSignal(audio, tolerance, progressCb, &debugOutput);
        computationTime
            +=std::chrono::duration_cast<std::chrono::milliseconds>(
                   std::chrono::steady_clock::now() - now);
        ProgressBar(progressBarWidth, 100 * count++ / numFiles);
        const auto expected = GetBpmFromFilename(wavFile);
        const auto truth = expected.has_value();
        const std::optional<OctaveError> error
            =truth && debugOutput.bpm > 0
              ? std::make_optional(GetOctaveError(*expected, debugOutput.bpm))
              : std::nullopt;
        sampleValueCsv << (truth ? "true" : "false") << ","
                       << debugOutput.score << ","
                       << 60. * debugOutput.tatumQuantization.numDivisions
            / debugOutput.audioFileDuration
                       << "," << debugOutput.bpm << ","
                       << ToString(debugOutput.timeSignature) << ","
                       << (error.has_value() ? error->factor : 0.) << ","
                       << (error.has_value() ? error->remainder : 0.) << ","
                       << debugOutput.tatumQuantization.lag << ","
                       << Pretty(wavFile) << "\n";
        return Sample { truth, debugOutput.score, error };
    });

    {
        std::ofstream timeMeasurementFile { "./timeMeasurement.txt" };
        timeMeasurementFile << computationTime.count() << "ms\n";
    }

    // AUC of ROC curve. Tells how good our loop/not-loop clasifier is.
    const auto rocInfo = GetRocInfo(
        samples, loopClassifierSettings.at(tolerance).allowedFalsePositiveRate);

    const auto strictThreshold
        =GetRocInfo(
              samples, loopClassifierSettings.at(FalsePositiveTolerance::Strict)
              .allowedFalsePositiveRate)
          .threshold;

    // Get RMS of octave errors. Tells how good the BPM estimation is.
    const auto octaveErrors = std::accumulate(
        samples.begin(), samples.end(), std::vector<double> {},
        [&](std::vector<double> octaveErrors, const Sample& sample)
    {
        if (sample.octaveError.has_value()) {
            octaveErrors.push_back(sample.octaveError->remainder);
        }
        return octaveErrors;
    });
    const auto octaveErrorStd = std::sqrt(
        std::accumulate(
            octaveErrors.begin(), octaveErrors.end(), 0.,
            [&](double sum, double octaveError)
    { return sum + octaveError * octaveError; })
        / octaveErrors.size());

    constexpr auto previousAuc = 0.9312244897959182;
    const auto classifierQualityHasChanged
        =std::abs(rocInfo.areaUnderCurve - previousAuc) >= 0.01;

    // Only update the summary if the figures have significantly changed.
    if (classifierQualityHasChanged) {
        std::ofstream summaryFile {
            std::string(CMAKE_CURRENT_SOURCE_DIR)
            + "/TatumQuantizationFitBenchmarkingOutput/summary.txt"
        };
        summaryFile << std::setprecision(
            std::numeric_limits<double>::digits10 + 1)
                    << "AUC: " << rocInfo.areaUnderCurve << "\n"
                    << "Strict Threshold (Minutes-and-Seconds): "
                    << strictThreshold << "\n"
                    << "Lenient Threshold (Beats-and-Measures): "
                    << rocInfo.threshold << "\n"
                    << "Octave error RMS: " << octaveErrorStd << "\n"
                    << "Audio file checksum: " << checksum << "\n";
        // Write sampleValueCsv to a file.
        std::ofstream sampleValueCsvFile {
            std::string(CMAKE_CURRENT_SOURCE_DIR)
            + "/TatumQuantizationFitBenchmarkingOutput/sampleValues.csv"
        };
        sampleValueCsvFile << sampleValueCsv.rdbuf();
    }

    // If this changed, then some non-refactoring code change happened. If
    // `rocInfo.areaUnderCurve > previousAuc`, then there's probably no argument
    // about the change. On the contrary, though, the change is either an
    // inadvertent bug, and if it is deliberate, should be well justified.
    REQUIRE(!classifierQualityHasChanged);
}
} // namespace MIR
