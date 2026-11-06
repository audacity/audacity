/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "au3-mixer/ISequenceProcessingNode.h"

class PlayableSequence;

namespace au::au4juce {
class IMuteSoloContext;

//! Its input, silent while its track is muted, or while another track solos; in place, in stereo
class MuteSoloNode final : public ISequenceProcessingNode
{
public:
    MuteSoloNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track,
                 const IMuteSoloContext& context);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const std::shared_ptr<const PlayableSequence> m_track;
    const IMuteSoloContext& m_context;
};
}
