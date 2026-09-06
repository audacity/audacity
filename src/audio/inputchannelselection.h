/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <vector>

namespace au::audio {
using InputChannelIndex = uint32_t;

struct InputChannelGroup {
    std::vector<InputChannelIndex> channels;
};

inline bool operator==(const InputChannelGroup& lhs, const InputChannelGroup& rhs)
{
    return lhs.channels == rhs.channels;
}

inline bool operator!=(const InputChannelGroup& lhs, const InputChannelGroup& rhs)
{
    return !(lhs == rhs);
}

using InputChannelSelection = std::vector<InputChannelGroup>;

inline size_t inputChannelCount(const InputChannelSelection& selection)
{
    size_t result = 0;
    for (const auto& group : selection) {
        result += group.channels.size();
    }
    return result;
}

inline InputChannelSelection legacyInputChannelSelection(int channels)
{
    if (channels <= 0) {
        return {};
    }
    if (channels == 1) {
        return { { { 0 } } };
    }
    if (channels == 2) {
        return { { { 0, 1 } } };
    }

    InputChannelSelection result;
    for (int channel = 0; channel < channels; ++channel) {
        result.push_back({ { static_cast<InputChannelIndex>(channel) } });
    }
    return result;
}

inline bool isValidInputChannelGroup(const InputChannelGroup& group, int availableChannels)
{
    if (availableChannels <= 0 || group.channels.empty() || group.channels.size() > 2) {
        return false;
    }

    if (group.channels.size() == 2
        && (group.channels[0] % 2 != 0 || group.channels[1] != group.channels[0] + 1)) {
        return false;
    }

    return std::all_of(group.channels.begin(), group.channels.end(), [availableChannels](InputChannelIndex channel) {
        return channel < static_cast<InputChannelIndex>(availableChannels);
    });
}

inline InputChannelSelection normalizeInputChannelSelection(InputChannelSelection selection, int availableChannels)
{
    if (availableChannels <= 0) {
        return {};
    }

    // Clamp exact count presets before discarding groups, so reducing N > 2
    // inputs to two preserves the legacy stereo layout. Custom groups stay separate.
    const auto selectedChannels = inputChannelCount(selection);
    if (selectedChannels > static_cast<size_t>(availableChannels)
        && selectedChannels <= static_cast<size_t>(std::numeric_limits<int>::max())
        && selection == legacyInputChannelSelection(static_cast<int>(selectedChannels))) {
        return legacyInputChannelSelection(availableChannels);
    }

    selection.erase(std::remove_if(selection.begin(), selection.end(), [availableChannels](const InputChannelGroup& group) {
        return !isValidInputChannelGroup(group, availableChannels);
    }), selection.end());
    std::sort(selection.begin(), selection.end(), [](const InputChannelGroup& lhs, const InputChannelGroup& rhs) {
        if (lhs.channels.front() != rhs.channels.front()) {
            return lhs.channels.front() < rhs.channels.front();
        }
        return lhs.channels.size() < rhs.channels.size();
    });

    InputChannelSelection result;
    std::vector<bool> used(static_cast<size_t>(availableChannels));
    for (auto& group : selection) {
        const bool overlaps = std::any_of(group.channels.begin(), group.channels.end(), [&used](InputChannelIndex channel) {
            return used[channel];
        });
        if (overlaps) {
            continue;
        }
        for (const auto channel : group.channels) {
            used[channel] = true;
        }
        result.push_back(std::move(group));
    }

    if (result.empty()) {
        result.push_back({ { 0 } });
    }
    return result;
}

inline InputChannelSelection availableInputChannelGroups(int availableChannels)
{
    InputChannelSelection result;
    for (int channel = 0; channel < availableChannels; ++channel) {
        result.push_back({ { static_cast<InputChannelIndex>(channel) } });
    }
    for (int channel = 0; channel + 1 < availableChannels; channel += 2) {
        result.push_back({ { static_cast<InputChannelIndex>(channel), static_cast<InputChannelIndex>(channel + 1) } });
    }
    return result;
}

inline InputChannelSelection toggleInputChannelGroup(
    const InputChannelSelection& selection, const InputChannelGroup& group, int availableChannels)
{
    auto result = normalizeInputChannelSelection(selection, availableChannels);
    if (!isValidInputChannelGroup(group, availableChannels)) {
        return result;
    }

    const auto exact = std::find(result.begin(), result.end(), group);
    if (exact != result.end()) {
        if (result.size() == 1) {
            return result;
        }
        result.erase(exact);
        return normalizeInputChannelSelection(std::move(result), availableChannels);
    }

    result.erase(std::remove_if(result.begin(), result.end(), [&group](const InputChannelGroup& candidate) {
        return std::any_of(candidate.channels.begin(), candidate.channels.end(), [&group](InputChannelIndex channel) {
            return std::find(group.channels.begin(), group.channels.end(), channel) != group.channels.end();
        });
    }), result.end());
    result.push_back(group);
    return normalizeInputChannelSelection(std::move(result), availableChannels);
}

inline std::vector<InputChannelIndex> flattenInputChannelSelection(const InputChannelSelection& selection)
{
    std::vector<InputChannelIndex> result;
    for (const auto& group : selection) {
        result.insert(result.end(), group.channels.begin(), group.channels.end());
    }
    return result;
}

inline size_t inputChannelStreamWidth(const InputChannelSelection& selection)
{
    InputChannelIndex highest = 0;
    bool found = false;
    for (const auto& group : selection) {
        for (const auto channel : group.channels) {
            highest = std::max(highest, channel);
            found = true;
        }
    }
    return found ? static_cast<size_t>(highest) + 1 : 0;
}
}
