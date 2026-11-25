/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <functional>
#include <memory>
#include <vector>

#include "au3-clip-processing/sequence-graph/ISequenceProcessingNode.h"

struct PlayableSequence;

namespace au::au4audio {
class IMuteSoloContext;

//! The mute and solo of a track or a bus, read as it renders
struct MuteSoloSwitches {
    std::function<bool()> mute;
    std::function<bool()> solo;
};

//! Its input, silent while its track is muted, or while another track solos; in place, in stereo
/*!
 A track routed with soloing ones plays too: the tracks a soloing bus mixes, and the bus that mixes a soloing track.
 */
class MuteSoloNode final : public ISequenceProcessingNode
{
public:
    //! @param soloedWith the solos of the tracks it is routed with
    MuteSoloNode(std::shared_ptr<ISequenceProcessingNode> input, MuteSoloSwitches switches, const IMuteSoloContext& context,
                 std::vector<std::function<bool()> > soloedWith = {});
    MuteSoloNode(std::shared_ptr<ISequenceProcessingNode> input, std::shared_ptr<const PlayableSequence> track,
                 const IMuteSoloContext& context, std::vector<std::function<bool()> > soloedWith = {});

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    const MuteSoloSwitches m_switches;
    const IMuteSoloContext& m_context;
    const std::vector<std::function<bool()> > m_soloedWith;
};
}
