/*
* Audacity: A Digital Audio Editor
*/
#include "effectscratch.h"

using namespace au::au4audio;

void EffectScratch::allocate(size_t frames)
{
    for (auto& buffer : m_buffers) {
        buffer.assign(frames, 0.f);
    }
    for (int i = 0; i < numChannels; ++i) {
        m_scratch[i] = m_buffers[i].data();
    }
}
