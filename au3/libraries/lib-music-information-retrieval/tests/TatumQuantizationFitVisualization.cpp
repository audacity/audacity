#include "MirFakes.h"
#include "MirTestUtils.h"
#include "MusicInformationRetrieval.h"
#include "WavMirAudioReader.h"

#include <catch2/catch.hpp>
#include <fstream>

namespace MIR {
TEST_CASE("TatumQuantizationFitVisualization")
{
    // This test produces python files containing data. Besides being useful for
    // debugging, after you have run this, you can run
    // `visualize_debug_output.py` to visualize the working of the algorithm, or
    // `visualize_post-processed_STFT.py` to visualize the STFT used to produce
    // the ODF.

    if (!runLocally) {
        return;
    }

    const auto wavFile
        =std::string { CMAKE_CURRENT_SOURCE_DIR }
    + "/benchmarking-dataset/loops/Acoustic Loop Lucaz Collab 116BPM.wav.mp3";
    const WavMirAudioReader audio { wavFile };
    QuantizationFitDebugOutput debugOutput;
    const auto result = GetMusicalMeterFromSignal(
        audio, FalsePositiveTolerance::Lenient, nullptr, &debugOutput);

    std::ofstream debug_output_module {
        std::string(CMAKE_CURRENT_SOURCE_DIR)
        + "/TatumQuantizationFitVisualization/debug_output.py"
    };
    debug_output_module << "wavFile = \"" << wavFile << "\"\n";
    debug_output_module << "odfSr = " << debugOutput.odfSr << "\n";
    debug_output_module << "audioFileDuration = "
                        << debugOutput.audioFileDuration << "\n";
    debug_output_module << "score = " << debugOutput.score << "\n";
    debug_output_module << "tatumRate = "
                        << 60. * debugOutput.tatumQuantization.numDivisions
        / debugOutput.audioFileDuration
                        << "\n";
    debug_output_module << "bpm = " << (result.has_value() ? result->bpm : 0.)
                        << "\n";
    debug_output_module << "lag = " << debugOutput.tatumQuantization.lag << "\n";
    debug_output_module << "odf_peak_indices = [";
    std::for_each(
        debugOutput.odfPeakIndices.begin(), debugOutput.odfPeakIndices.end(),
        [&](int i) { debug_output_module << i << ","; });
    debug_output_module << "]\n";
    PrintPythonVector(debug_output_module, debugOutput.odf, "odf");
    PrintPythonVector(debug_output_module, debugOutput.rawOdf, "rawOdf");
    PrintPythonVector(
        debug_output_module, debugOutput.movingAverage, "movingAverage");
    PrintPythonVector(
        debug_output_module, debugOutput.odfAutoCorr, "odfAutoCorr");
    PrintPythonVector(
        debug_output_module, debugOutput.odfAutoCorrPeakIndices,
        "odfAutoCorrPeakIndices");

    std::ofstream stft_log_module {
        std::string { CMAKE_CURRENT_SOURCE_DIR }
        + "/TatumQuantizationFitVisualization/stft_log.py"
    };
    stft_log_module << "wavFile = \"" << wavFile << "\"\n";
    stft_log_module << "sampleRate = " << audio.GetSampleRate() << "\n";
    stft_log_module << "frameRate = " << debugOutput.odfSr << "\n";
    stft_log_module << "stft = [";
    std::for_each(
        debugOutput.postProcessedStft.begin(),
        debugOutput.postProcessedStft.end(), [&](const auto& row) {
        stft_log_module << "[";
        std::for_each(row.begin(), row.end(), [&](float x) {
            stft_log_module << x << ",";
        });
        stft_log_module << "],";
    });
    stft_log_module << "]\n";
}
} // namespace MIR
