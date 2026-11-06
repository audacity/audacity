/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-mixer/ISequenceProcessingNode.h"

#include <memory>
#include <vector>

class ClipInterface;
class WideSampleSequence;
using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface> >;

namespace SequenceProcessingGraphFactory {
//! The graph processing a sequence as it is played: its end node, which owns the others. It reads forward only.
//! Must be called on the main thread.
std::unique_ptr<ISequenceProcessingNode> CreateGraph(const WideSampleSequence&, const ClipConstHolders& clips);
}
