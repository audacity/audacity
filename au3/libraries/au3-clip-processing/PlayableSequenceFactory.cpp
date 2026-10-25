/*
 * Audacity: A Digital Audio Editor
 */
#include "PlayableSequenceFactory.h"
#include "clip-graph/ClipProcessingGraphFactory.h"
#include "sequence-graph/ClipTimelineNode.h"
#include "PlayableSequenceImpl.h"

std::shared_ptr<PlayableSequence> PlayableSequenceFactory::Create(
    const PlayableSequence& sequence, const ClipConstHolders& clips)
{
    const auto numChannels = sequence.NChannels();
    const auto rate = sequence.GetRate();
    std::vector<ClipTimelineNode::ClipAndGraph> clipGraphs;
    clipGraphs.reserve(clips.size());
    for (const auto& clip : clips) {
        clipGraphs.push_back({ clip, ClipProcessingGraphFactory::CreateGraph(*clip) });
    }
    auto graphEndNode = std::make_unique<ClipTimelineNode>(std::move(clipGraphs), numChannels, rate);
    return std::make_shared<PlayableSequenceImpl>(sequence, std::move(graphEndNode));
}
