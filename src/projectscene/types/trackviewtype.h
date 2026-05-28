/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::projectscene {
enum class TrackViewType : int {
    Undefined = -1,
    Waveform,
    Spectrogram,
    WaveformAndSpectrogram,
};
}
