/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

class SpectrogramSettings;

namespace au::spectrogram {
struct SpectrogramTrackContext {
    ::SpectrogramSettings& settings;
    const bool trackIsSelected;
    const float minFreq;
    const float maxFreq;
};
}
