/*
 * Audacity: A Digital Audio Editor
 */
#include "SequenceProcessingGraphFactory.h"
#include "clip-graph/ClipProcessingGraphFactory.h"
#include "sequence-graph/ClipTimelineNode.h"
#include "sequence-graph/CrossFadingSmoother.h"

#include "au3-mixer/WideSampleSequence.h"

std::unique_ptr<ISequenceProcessingNode> SequenceProcessingGraphFactory::CreateGraph(
    const WideSampleSequence& sequence, const ClipConstHolders& clips)
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
    return std::make_unique<CrossFadingSmoother>(std::move(timeline), numChannels, lookahead);
}
