/*
 * Audacity: A Digital Audio Editor
 */
#include "PlayableSequenceImpl.h"
#include "sequence-graph/ISequenceProcessingNode.h"

#include <algorithm>
#include <cassert>

PlayableSequenceImpl::PlayableSequenceImpl(const PlayableSequence& sequence, std::unique_ptr<ISequenceProcessingNode> graphEndNode)
    : mSequence{sequence}
    , mGraphEndNode{std::move(graphEndNode)}
{
}

size_t PlayableSequenceImpl::NChannels() const
{
    return mSequence.NChannels();
}

float PlayableSequenceImpl::GetChannelVolume(int channel) const
{
    return mSequence.GetChannelVolume(channel);
}

const ChannelGroup* PlayableSequenceImpl::FindChannelGroup() const
{
    return mSequence.FindChannelGroup();
}

bool PlayableSequenceImpl::GetSolo() const
{
    return mSequence.GetSolo();
}

bool PlayableSequenceImpl::GetMute() const
{
    return mSequence.GetMute();
}

double PlayableSequenceImpl::GetStartTime() const
{
    return mSequence.GetStartTime();
}

double PlayableSequenceImpl::GetEndTime() const
{
    return mSequence.GetEndTime();
}

double PlayableSequenceImpl::GetRate() const
{
    return mSequence.GetRate();
}

sampleFormat PlayableSequenceImpl::WidestEffectiveFormat() const
{
    return mSequence.WidestEffectiveFormat();
}

bool PlayableSequenceImpl::HasTrivialEnvelope() const
{
    return mSequence.HasTrivialEnvelope();
}

void PlayableSequenceImpl::GetEnvelopeValues(
    double* buffer, size_t bufferLen, double t0, bool backwards) const
{
    mSequence.GetEnvelopeValues(buffer, bufferLen, t0, backwards);
}

AudioGraph::ChannelType PlayableSequenceImpl::GetChannelType() const
{
    return mSequence.GetChannelType();
}

bool PlayableSequenceImpl::DoGet(
    size_t iChannel, size_t nBuffers, const samplePtr buffers[],
    sampleFormat format, sampleCount start, size_t len, bool backwards,
    fillFormat, bool, sampleCount*) const
{
    // PlayableSequenceImpl is not expected to be used for any other case.
    assert(iChannel == 0u);
    assert(format == floatSample);
    // More-than-stereo isn't supported
    assert(nBuffers <= 2);

    // Reading backwards, e.g. for scrubbing, isn't supported
    assert(!backwards);
    if (backwards) {
        for (auto i = 0u; i < nBuffers; ++i) {
            std::fill_n(reinterpret_cast<float*>(buffers[i]), len, 0.f);
        }
        return true;
    }

    assert(nBuffers == NChannels());
    mGraphEndNode->GetSamples(reinterpret_cast<float* const*>(buffers), start, len);
    return true;
}
