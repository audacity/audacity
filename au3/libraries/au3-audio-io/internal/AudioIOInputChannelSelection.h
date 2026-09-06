/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <vector>

namespace audacity::audio_io::details {
using InputChannelSelection = std::vector<std::vector<unsigned int> >;

inline InputChannelSelection LegacyInputChannelSelection(size_t channels)
{
    if (channels == 1) {
        return { { 0 } };
    }
    if (channels == 2) {
        return { { 0, 1 } };
    }

    InputChannelSelection result;
    for (size_t channel = 0; channel < channels; ++channel) {
        result.push_back({ static_cast<unsigned int>(channel) });
    }
    return result;
}

inline void MixInputChannelSelectionToStereo(
    const float* inputSamples, size_t inputStreamChannels,
    const InputChannelSelection& selection,
    float* outputBuffer, size_t frames)
{
    if (!inputSamples || inputStreamChannels == 0 || selection.empty()
        || !outputBuffer) {
        return;
    }

    const auto divisor = static_cast<float>(selection.size());
    for (size_t frame = 0; frame < frames; ++frame) {
        float left = 0.0f;
        float right = 0.0f;
        const auto* frameSamples = inputSamples + frame * inputStreamChannels;
        for (const auto& group : selection) {
            if (group.size() == 1) {
                left += frameSamples[group[0]];
                right += frameSamples[group[0]];
            } else if (group.size() == 2) {
                left += frameSamples[group[0]];
                right += frameSamples[group[1]];
            }
        }
        outputBuffer[2 * frame] = std::clamp(left / divisor, -1.0f, 1.0f);
        outputBuffer[2 * frame + 1] = std::clamp(right / divisor, -1.0f, 1.0f);
    }
}

}
