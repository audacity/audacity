/*
* Audacity: A Digital Audio Editor
*/
#include "blockcontext.h"

#include <algorithm>

#include "au3-project/Project.h"

#include "effectscratch.h"
#include "effectslatency.h"
#include "playheadmapper.h"
#include "stereo.h"

using namespace au::au4audio;

BlockContext::BlockContext(std::weak_ptr<AudacityProject> project, RealtimeEffects::InitializationScope& effects,
                           ConstPlayableSequences sequences, const PlayheadMapper& mapper, const std::atomic<int64_t>& playEnd,
                           double sampleRate)
    : m_project{std::move(project)}, m_effects{effects}, m_sequences{std::move(sequences)}, m_mapper{mapper},
    m_playEndPosition{playEnd}, m_sampleRate{sampleRate}
{
}

void BlockContext::begin()
{
    m_scope.emplace(m_effects, m_project);
    m_anySoloing = std::any_of(m_sequences.begin(), m_sequences.end(), [](const auto& sequence) { return sequence->GetSolo(); });
    // Unless looping, the tracks go silent at the end
    if (m_mapper.loop()) {
        m_playEnd.reset();
    } else {
        m_playEnd = m_playEndPosition.load();
    }
}

void BlockContext::end()
{
    m_scope.reset();
}

int BlockContext::latency(const ChannelGroup* group) const
{
    const auto project = m_project.lock();
    return project ? effectsLatency(*project, group, m_sampleRate) : 0;
}

void BlockContext::process(const ChannelGroup* group, float* const* buffers, EffectScratch& scratch, size_t frames)
{
    if (m_scope) {
        // Ignores how many samples to discard for the effects' latency: EffectsNode compensates it
        m_scope->Process(group, buffers, scratch.scratch(), scratch.dummy(), numChannels, frames);
    }
}

BlockContextNode::BlockContextNode(std::shared_ptr<ISequenceProcessingNode> input, BlockContext& context)
    : m_input{std::move(input)}, m_context{context}
{
}

void BlockContextNode::GetSamples(float* const* buffers, sampleCount start, size_t len)
{
    m_context.begin();
    m_input->GetSamples(buffers, start, len);
    m_context.end();
}
