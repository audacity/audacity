/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::spectrogram {
// Spectrogram
enum class SpectrogramScale {
    Undefined = -1,
    Linear,
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

struct SelectionInfo {
    static constexpr int UndefinedFrequency = -1;

    const double startTime = 0.0;
    const double endTime = 0.0;
    const double startFrequency = UndefinedFrequency;
    const double endFrequency = UndefinedFrequency;
};

struct ClipChannelInfo {
    const int clipId = -1;
    const int trackId = -1;
    const int channel = -1;
    const int xPaintBegin = 0;
    const int xPaintEnd = 0;
};

struct ViewInfo {
    const double channelHeight = 0.;
    const double viewportStartTime = 0.;
    const double viewportEndTime = 0.;
    const double pixelsPerSecond = 0.; // aka zoom
};

struct FrequencySelection {
    FrequencySelection() = default;
    FrequencySelection(int trackId, double f1, double f2)
        : trackId(trackId), startFrequency(std::min(f1, f2)), endFrequency(std::max(f1, f2)) {}

    int trackId = -1;
    double startFrequency = SelectionInfo::UndefinedFrequency;
    double endFrequency = SelectionInfo::UndefinedFrequency;

    constexpr bool isValid() const
    {
        return trackId != -1
               && startFrequency != SelectionInfo::UndefinedFrequency
               && endFrequency != SelectionInfo::UndefinedFrequency;
    }

    constexpr bool operator==(const FrequencySelection& other) const
    {
        return trackId == other.trackId
               && startFrequency == other.startFrequency
               && endFrequency == other.endFrequency;
    }

    constexpr bool operator!=(const FrequencySelection& other) const
    {
        return !(*this == other);
    }
};

struct SpectrogramRulerTick {
    double val = 0.f;
    double pos = 0.f;
};

struct SpectrogramRulerTicks {
    std::vector<SpectrogramRulerTick> major;
    std::vector<SpectrogramRulerTick> minor;
};
}
