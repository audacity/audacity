/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <algorithm>
#include <limits>
#include <utility>

#include "framework/global/types/val.h"

#include "audio/inputchannelselection.h"

namespace au::au3audio::details {
inline audio::InputChannelSelection inputChannelSelectionFromVal(
    const muse::Val& value)
{
    audio::InputChannelSelection result;
    for (const auto& groupValue : value.toList()) {
        audio::InputChannelGroup group;
        bool valid = true;
        for (const auto& channelValue : groupValue.toList()) {
            if (channelValue.type() != muse::Val::Type::Int
                && channelValue.type() != muse::Val::Type::Int64) {
                valid = false;
                break;
            }
            const int64_t channel = channelValue.toInt64();
            if (channel < 0
                || channel > std::numeric_limits<audio::InputChannelIndex>::max()) {
                valid = false;
                break;
            }
            group.channels.push_back(
                static_cast<audio::InputChannelIndex>(channel));
        }
        if (valid && !group.channels.empty()) {
            result.push_back(std::move(group));
        }
    }
    return result;
}

inline muse::Val inputChannelSelectionToVal(
    const audio::InputChannelSelection& selection)
{
    muse::ValList groups;
    for (const auto& group : selection) {
        muse::ValList channels;
        for (const auto channel : group.channels) {
            channels.emplace_back(static_cast<int>(channel));
        }
        groups.emplace_back(channels);
    }
    return muse::Val(groups);
}

inline audio::InputChannelSelection inputChannelSelectionFromSettings(
    const muse::Val& value, int legacyChannels, int availableChannels)
{
    auto selection = inputChannelSelectionFromVal(value);
    if (selection.empty()) {
        return audio::legacyInputChannelSelection(
            availableChannels > 0 ? std::clamp(legacyChannels, 1, availableChannels) : 0);
    }
    return audio::normalizeInputChannelSelection(
        std::move(selection), availableChannels);
}
}
