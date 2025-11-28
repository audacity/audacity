/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::spectrogram {
// Spectrogram
enum class SpectrogramScale {
    Linear = 0,
    Logarithmic,
    Mel,
    Bark,
    ERB,
    Period,
    _count
};

enum class SpectrogramColorScheme {
    Roseus = 0,
    Classic,
    Grayscale,
    InverseGrayscale,
    _count
};

enum class SpectrogramAlgorithm {
    Frequencies = 0,
    Reassignment,
    Pitch,
    _count
};

enum class SpectrogramWindowType {
    Rectangular = 0,
    Bartlett,
    Hamming,
    Hann,
    Blackman,
    BlackmanHarris,
    Welch,
    Gaussian25,
    Gaussian35,
    Gaussian45,
    _count
};

struct AllSpectrogramSettings {
    bool spectralSelectionEnabled = false;
    SpectrogramColorScheme colorScheme = static_cast<SpectrogramColorScheme>(0);
    int colorGainDb = 0;
    int colorRangeDb = 0;
    int colorHighBoostDbPerDec = 0;
    SpectrogramScale scale = static_cast<SpectrogramScale>(0);
    SpectrogramAlgorithm algorithm = static_cast<SpectrogramAlgorithm>(0);
    SpectrogramWindowType windowType = static_cast<SpectrogramWindowType>(0);
    int winSizeLog2 = 0;
    int zeroPaddingFactor = 0;
};
}
