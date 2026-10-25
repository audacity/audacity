/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipProcessingGraphFactory.h"
#include "ClipBufferingNode.h"
#include "ClipStretchingNode.h"

std::unique_ptr<IClipProcessingNode> ClipProcessingGraphFactory::CreateGraph(const ClipInterface& clip, size_t lookaheadSamples)
{
    auto stretched = std::make_unique<ClipStretchingNode>(clip);
    // Samples read again are not stretched again
    return std::make_unique<ClipBufferingNode>(std::move(stretched), lookaheadSamples);
}
