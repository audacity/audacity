/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <vector>

class ClipInterface;
struct PlayableSequence;
using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface> >;

namespace PlayableSequenceFactory {
//! Must be called on the main thread
std::shared_ptr<PlayableSequence> Create(const PlayableSequence&, const ClipConstHolders& clips);
}
