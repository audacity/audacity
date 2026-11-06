/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>

class ChannelGroup;

namespace au::au4juce {
class EffectScratch;

//! What an EffectsNode needs to process a group's realtime effects
class IEffectsContext
{
public:
    virtual ~IEffectsContext() = default;

    //! How late the effects of `group` (a track's, or RealtimeEffectManager::MasterGroup) output what they're given, in
    //! samples
    virtual int latency(const ChannelGroup* group) const = 0;

    //! Processes `frames` frames of the stereo `buffers` in place through the effects of `group`, in the current block's
    //! processing scope, with `scratch` as their working space. Any thread rendering the block; `scratch` is the
    //! caller's own.
    virtual void process(const ChannelGroup* group, float* const* buffers, EffectScratch& scratch, size_t frames) = 0;
};
}
