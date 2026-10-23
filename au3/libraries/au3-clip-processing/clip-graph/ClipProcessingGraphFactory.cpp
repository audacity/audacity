/*
 * Audacity: A Digital Audio Editor
 */
#include "ClipProcessingGraphFactory.h"
#include "ClipBufferingNode.h"
#include "ClipFades.h"
#include "ClipFadesNode.h"
#include "ClipInterface.h"
#include "ClipStretchingNode.h"

std::unique_ptr<IClipProcessingNode> ClipProcessingGraphFactory::CreateGraph(const ClipInterface& clip, size_t lookaheadSamples)
{
    auto stretched = std::make_unique<ClipStretchingNode>(clip);
    // Samples read again are faded again, but not stretched again
    auto buffered = std::make_unique<ClipBufferingNode>(std::move(stretched), lookaheadSamples);
    // Getting the fades may construct them, which must not happen on the audio thread
    auto faded = std::make_unique<ClipFadesNode>(std::move(buffered), ClipFades::Get(clip), clip.GetRate());
    return faded;
}
