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

inline bool IsStructurallyValidInputChannelSelection(
    const InputChannelSelection& selection)
{
    std::vector<unsigned int> usedChannels;
    for (const auto& group : selection) {
        if (group.empty() || group.size() > 2) {
            return false;
        }
        for (const auto channel : group) {
            if (std::find(usedChannels.begin(), usedChannels.end(), channel) != usedChannels.end()) {
                return false;
            }
            usedChannels.push_back(channel);
        }
    }
    return true;
}

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

inline size_t InputChannelSelectionCount(const InputChannelSelection& selection)
{
    size_t result = 0;
    for (const auto& group : selection) {
        result += group.size();
    }
    return result;
}

inline std::vector<unsigned int> FlattenInputChannelSelection(
    const InputChannelSelection& selection)
{
    std::vector<unsigned int> result;
    result.reserve(InputChannelSelectionCount(selection));
    for (const auto& group : selection) {
        result.insert(result.end(), group.begin(), group.end());
    }
    return result;
}

inline size_t InputChannelSelectionStreamWidth(
    const InputChannelSelection& selection)
{
    size_t result = 0;
    for (const auto& group : selection) {
        for (const auto channel : group) {
            result = std::max(result, static_cast<size_t>(channel) + 1);
        }
    }
    return result;
}

template<typename Sample>
inline bool CopyInputChannel(
    const Sample* inputSamples, size_t inputStreamChannels,
    unsigned int inputChannel, Sample* outputSamples, size_t frames)
{
    if (!inputSamples || !outputSamples || inputStreamChannels == 0
        || inputChannel >= inputStreamChannels) {
        return false;
    }

    for (size_t frame = 0; frame < frames; ++frame) {
        outputSamples[frame]
            = inputSamples[frame * inputStreamChannels + inputChannel];
    }
    return true;
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

inline float InputChannelSelectionPeak(
    const float* inputSamples, size_t inputStreamChannels,
    const std::vector<unsigned int>& inputChannelIndices, size_t frames)
{
    if (!inputSamples || inputStreamChannels == 0) {
        return 0.0f;
    }

    float maxPeak = 0.0f;
    for (size_t frame = 0; frame < frames; ++frame) {
        for (const auto channel : inputChannelIndices) {
            if (channel >= inputStreamChannels) {
                continue;
            }
            maxPeak = std::max(
                maxPeak,
                std::fabs(inputSamples[frame * inputStreamChannels + channel]));
        }
    }
    return maxPeak;
}
}
