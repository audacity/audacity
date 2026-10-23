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
//! Duration over which fade length changes are applied, to avoid clicks when fades are dragged during playback
constexpr auto fadeLengthRampDuration = 0.02;

double FadeGain(
    double t, double clipStart, double clipEnd, double fadeIn, double fadeOut, FadeType fadeInType, FadeType fadeOutType)
{
    auto gain = 1.0;
    if (fadeIn > 0 && t < clipStart + fadeIn) {
        gain *= ClipFades::fadeGain(fadeInType, (t - clipStart) / fadeIn);
    }
    if (fadeOut > 0 && t > clipEnd - fadeOut) {
        gain *= ClipFades::fadeGain(fadeOutType, (clipEnd - t) / fadeOut);
    }
    return gain;
}
}

double FadingSequence::FadeLengthRamp::At(size_t i) const
{
    return i < remaining ? value + step * i : target;
}

void FadingSequence::FadeLengthRamp::Retarget(double newTarget, size_t rampLength)
{
    if (newTarget == target) {
        return;
    }
    // Ramp from where we are, which may be in the middle of a previous ramp
    target = newTarget;
    remaining = std::max<size_t>(rampLength, 1);
    step = (target - value) / remaining;
}

void FadingSequence::FadeLengthRamp::Advance(size_t numSamples)
{
    if (numSamples >= remaining) {
        value = target;
        remaining = 0;
    } else {
        value += step * numSamples;
        remaining -= numSamples;
    }
}

void FadingSequence::FadeLengthRamp::Snap(double newTarget)
{
    value = target = newTarget;
    remaining = 0;
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
    const auto rampLength = static_cast<size_t>(fadeLengthRampDuration * rate);

    for (auto& [clip, fades, fadeIn, fadeOut] : mClips) {
        if (continuesPreviousRead) {
            fadeIn.Retarget(fades.fadeIn(), rampLength);
            fadeOut.Retarget(fades.fadeOut(), rampLength);
        } else {
            fadeIn.Snap(fades.fadeIn());
            fadeOut.Snap(fades.fadeOut());
        }

        const auto clipStart = clip->GetPlayStartTime();
        const auto clipEnd = clip->GetPlayEndTime();
        const auto fadeInType = fades.fadeInType();
        const auto fadeOutType = fades.fadeOutType();
        const auto n0 = std::max(sampleCount { std::ceil(clipStart * rate) }, first);
        const auto n1 = std::min(sampleCount { std::ceil(clipEnd * rate) }, end);
        for (auto n = n0; n < n1; ++n) {
            // Index in the buffers, which is also the number of samples read so far
            const auto i = (backwards ? start - 1 - n : n - start).as_size_t();
            const auto gain = FadeGain(n.as_double() / rate, clipStart, clipEnd, fadeIn.At(i), fadeOut.At(i), fadeInType, fadeOutType);
            if (gain == 1.0) {
                continue;
            }
            for (size_t c = 0; c < nBuffers; ++c) {
                reinterpret_cast<float*>(buffers[c])[i] *= static_cast<float>(gain);
            }
        }

        fadeIn.Advance(len);
        fadeOut.Advance(len);
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
