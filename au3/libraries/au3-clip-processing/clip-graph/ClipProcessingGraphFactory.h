/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>

class ClipInterface;
class IClipProcessingNode;

namespace ClipProcessingGraphFactory {
//! Must be called on the main thread
std::unique_ptr<IClipProcessingNode> CreateGraph(const ClipInterface&);
}
