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

namespace {
//! Duration over which fade changes are applied, to avoid clicks when fades are dragged during playback
constexpr auto fadeChangeRampDuration = 0.02;
}

void FadingSequence::ApplyFades(
    size_t nBuffers, const samplePtr buffers[], sampleCount start, size_t len, bool backwards) const
{
    const auto rate = mSequence->GetRate();
    // Samples in the buffers: forward, from `start` (inclusive) to `start + len`;
    // backward, from `start` (exclusive) to `start - len`, in reverse order.
    const auto first = backwards ? start - static_cast<sampleCount::type>(len) : start;
    const auto end = first + static_cast<sampleCount::type>(len);

    const auto continuesPreviousRead = mExpectedStart == start && mExpectedBackwards == backwards;
    mExpectedStart = backwards ? first : end;
    mExpectedBackwards = backwards;
    const auto rampLength = std::max<size_t>(fadeChangeRampDuration * rate, 1);

    for (auto& [clip, fades, from, to, rampPosition] : mClips) {
        const auto latest = fades.snapshot();
        if (!continuesPreviousRead) {
            from = to = latest;
            rampPosition = rampLength;
        }

        const auto clipStart = clip->GetPlayStartTime();
        const auto n0 = sampleCount { std::ceil(clipStart * rate) };
        const auto n1 = sampleCount { std::ceil(clip->GetPlayEndTime() * rate) };

        // In reading order, since the ramps are in time of reading
        for (size_t i = 0; i < len; ++i, rampPosition = std::min(rampPosition + 1, rampLength)) {
            if (rampPosition == rampLength && latest != to) {
                from = to;
                to = latest;
                rampPosition = 0;
            }

            const auto n = backwards ? start - 1 - static_cast<sampleCount::type>(i) : start + static_cast<sampleCount::type>(i);
            if (n < n0 || n >= n1) {
                continue;
            }

            const auto t = n.as_double() / rate - clipStart;
            const auto gain = rampPosition < rampLength
                              ? std::lerp(from.gainAt(t), to.gainAt(t), static_cast<double>(rampPosition) / rampLength)
                              : to.gainAt(t);
            if (gain == 1.0) {
                continue;
            }
            for (size_t c = 0; c < nBuffers; ++c) {
                reinterpret_cast<float*>(buffers[c])[i] *= static_cast<float>(gain);
            }
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
