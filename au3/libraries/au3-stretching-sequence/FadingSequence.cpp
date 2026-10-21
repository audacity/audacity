/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FadingSequence.cpp

**********************************************************************/
#include "FadingSequence.h"

#include "ClipFades.h"
#include "StretchingSequence.h"

#include <algorithm>
#include <cassert>
#include <cmath>

std::shared_ptr<FadingSequence> FadingSequence::Create(
    const PlayableSequence& sequence, const ClipConstHolders& clips)
{
    return std::make_shared<FadingSequence>(StretchingSequence::Create(sequence, clips), clips);
}

FadingSequence::FadingSequence(std::shared_ptr<const PlayableSequence> sequence, const ClipConstHolders& clips)
    : mSequence{std::move(sequence)}
{
    // Get the fades now: `ClipFades::Get` may construct them, which must not happen on the audio thread.
    mClips.reserve(clips.size());
    for (const auto& clip : clips) {
        mClips.push_back({ clip, ClipFades::Get(*clip) });
    }
}

size_t FadingSequence::NChannels() const
{
    return mSequence->NChannels();
}

float FadingSequence::GetChannelVolume(int channel) const
{
    return mSequence->GetChannelVolume(channel);
}

double FadingSequence::GetStartTime() const
{
    return mSequence->GetStartTime();
}

double FadingSequence::GetEndTime() const
{
    return mSequence->GetEndTime();
}

double FadingSequence::GetRate() const
{
    return mSequence->GetRate();
}

sampleFormat FadingSequence::WidestEffectiveFormat() const
{
    return mSequence->WidestEffectiveFormat();
}

bool FadingSequence::HasTrivialEnvelope() const
{
    return mSequence->HasTrivialEnvelope();
}

void FadingSequence::GetEnvelopeValues(
    double* buffer, size_t bufferLen, double t0, bool backwards) const
{
    mSequence->GetEnvelopeValues(buffer, bufferLen, t0, backwards);
}

bool FadingSequence::DoGet(
    size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backwards,
    fillFormat fill, bool mayThrow, sampleCount* pNumWithinClips) const
{
    if (!mSequence->DoGet(iChannel, nBuffers, buffers, format, start, len, backwards, fill, mayThrow, pNumWithinClips)) {
        return false;
    }
    // Like StretchingSequence, which we decorate, we only expect float samples.
    assert(format == floatSample);
    if (format == floatSample) {
        ApplyFades(nBuffers, buffers, start, len, backwards);
    }
    return true;
}

void FadingSequence::ApplyFades(
    size_t nBuffers, const samplePtr buffers[], sampleCount start, size_t len, bool backwards) const
{
    const auto rate = mSequence->GetRate();
    // Samples in the buffers: forward, from `start` (inclusive) to `start + len`;
    // backward, from `start` (exclusive) to `start - len`, in reverse order.
    const auto first = backwards ? start - static_cast<sampleCount::type>(len) : start;
    const auto end = first + static_cast<sampleCount::type>(len);

    // Multiplies the samples whose times lie in [t0, t1) by `gain(t)`
    const auto applyGain = [&](double t0, double t1, const auto& gain) {
        const auto n0 = std::max(sampleCount { std::ceil(t0 * rate) }, first);
        const auto n1 = std::min(sampleCount { std::ceil(t1 * rate) }, end);
        for (auto n = n0; n < n1; ++n) {
            const auto i = (backwards ? start - 1 - n : n - start).as_size_t();
            const auto g = static_cast<float>(gain(n.as_double() / rate));
            for (size_t c = 0; c < nBuffers; ++c) {
                reinterpret_cast<float*>(buffers[c])[i] *= g;
            }
        }
    };

    for (const auto& [clip, fades] : mClips) {
        const auto clipStart = clip->GetPlayStartTime();
        const auto clipEnd = clip->GetPlayEndTime();
        if (clipEnd <= first.as_double() / rate || clipStart >= end.as_double() / rate) {
            continue;
        }

        // Linear fades for now
        if (const auto fadeIn = fades.fadeIn(); fadeIn > 0) {
            applyGain(clipStart, clipStart + fadeIn, [&](double t) { return (t - clipStart) / fadeIn; });
        }
        if (const auto fadeOut = fades.fadeOut(); fadeOut > 0) {
            applyGain(clipEnd - fadeOut, clipEnd, [&](double t) { return (clipEnd - t) / fadeOut; });
        }
    }
}

const ChannelGroup* FadingSequence::FindChannelGroup() const
{
    return mSequence->FindChannelGroup();
}

bool FadingSequence::GetSolo() const
{
    return mSequence->GetSolo();
}

bool FadingSequence::GetMute() const
{
    return mSequence->GetMute();
}

AudioGraph::ChannelType FadingSequence::GetChannelType() const
{
    return mSequence->GetChannelType();
}
