/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <cstddef>
#include <vector>

#include "stereo.h"

namespace au::au4audio {
//! Scratch space in the layout RealtimeEffectManager::Process() expects
class EffectScratch
{
public:
    void allocate(size_t frames);

    float* const* scratch() { return m_scratch; }
    float* dummy() { return m_buffers[numChannels].data(); }

private:
    std::vector<float> m_buffers[numChannels + 1];
    float* m_scratch[numChannels] {};
};
}
