/*
 * Audacity: A Digital Audio Editor
 */
#include "PlayableSequenceFactory.h"
#include "clip-graph/ClipProcessingGraphFactory.h"
#include "sequence-graph/ClipTimelineNode.h"
#include "sequence-graph/CrossFadingSmoother.h"
#include "PlayableSequenceImpl.h"

std::shared_ptr<PlayableSequence> PlayableSequenceFactory::Create(
    const PlayableSequence& sequence, const ClipConstHolders& clips)
{
    const auto numChannels = sequence.NChannels();
    const auto rate = sequence.GetRate();
    const auto lookahead = static_cast<size_t>(CrossFadingSmoother::smootherCrossfadeDuration * rate + 0.5);
    std::vector<ClipTimelineNode::ClipAndGraph> clipGraphs;
    clipGraphs.reserve(clips.size());
    for (const auto& clip : clips) {
        clipGraphs.push_back({ clip, ClipProcessingGraphFactory::CreateGraph(*clip, lookahead) });
    }
    auto timeline = std::make_unique<ClipTimelineNode>(std::move(clipGraphs), numChannels, rate);
    auto graphEndNode = std::make_unique<CrossFadingSmoother>(std::move(timeline), numChannels, lookahead);
    return std::make_shared<PlayableSequenceImpl>(sequence, std::move(graphEndNode));
}
