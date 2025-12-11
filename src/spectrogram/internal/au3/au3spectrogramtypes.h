/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

namespace au::spectrogram {
class SpectrogramSettings;
struct SpectrogramTrackContext {
    SpectrogramSettings& settings;
    const bool trackIsSelected;
    const float minFreq;
    const float maxFreq;
};
}
