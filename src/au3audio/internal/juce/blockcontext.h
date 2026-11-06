/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <atomic>
#include <cstdint>
#include <memory>
#include <optional>

#include "au3-mixer/AudioIOSequences.h"
#include "au3-mixer/ISequenceProcessingNode.h"
#include "au3-realtime-effects/RealtimeEffectManager.h"

#include "ieffectscontext.h"
#include "iendoftrackcontext.h"
#include "imutesolocontext.h"

class AudacityProject;

namespace au::au4juce {
class PlayheadMapper;

//! The state of the block being rendered, which the chains' nodes share: set by BlockContextNode, around each block
class BlockContext final : public IEffectsContext, public IMuteSoloContext, public IEndOfTrackContext
{
public:
    //! @param effects where the effects of the sequences and of the master are initialized; must outlive this
    //! @param playEnd where the play range ends, in samples; must outlive this
    BlockContext(std::weak_ptr<AudacityProject> project, RealtimeEffects::InitializationScope& effects,
                 ConstPlayableSequences sequences, const PlayheadMapper& mapper, const std::atomic<int64_t>& playEnd,
                 double sampleRate);

    //! The rendering thread, before and after a block
    void begin();
    void end();

    // IEffectsContext
    int latency(const ChannelGroup* group) const override;
    void process(const ChannelGroup* group, float* const* buffers, EffectScratch& scratch, size_t frames) override;

    // IMuteSoloContext
    bool anySoloing() const override { return m_anySoloing; }

    // IEndOfTrackContext
    std::optional<int64_t> playEnd() const override { return m_playEnd; }

private:
    const std::weak_ptr<AudacityProject> m_project;
    RealtimeEffects::InitializationScope& m_effects;
    const ConstPlayableSequences m_sequences;
    const PlayheadMapper& m_mapper;
    const std::atomic<int64_t>& m_playEndPosition;
    const double m_sampleRate;

    // Set by begin(), for the block
    std::optional<RealtimeEffects::ProcessingScope> m_scope;
    bool m_anySoloing = false;
    std::optional<int64_t> m_playEnd;
};

//! Its input, rendered as a block: the context is set around each read
class BlockContextNode final : public ISequenceProcessingNode
{
public:
    BlockContextNode(std::shared_ptr<ISequenceProcessingNode> input, BlockContext& context);

    void GetSamples(float* const* buffers, sampleCount start, size_t len) override;

private:
    const std::shared_ptr<ISequenceProcessingNode> m_input;
    BlockContext& m_context;
};
}
