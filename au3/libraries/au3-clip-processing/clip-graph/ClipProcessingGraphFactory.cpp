/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipProcessingGraphFactory.h"
#include "ClipStretchingNode.h"

std::unique_ptr<IClipProcessingNode> ClipProcessingGraphFactory::CreateGraph(const ClipInterface& clip)
{
    return std::make_unique<ClipStretchingNode>(clip);
}
