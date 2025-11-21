/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AudioSegmentFactory.cpp

  Matthieu Hodgkinson

**********************************************************************/

#include "AudioSegmentFactory.h"
#include "ClipInterface.h"
#include "ClipSegment.h"
#include "SilenceSegment.h"
#include "TimeAndPitchInterface.h"

#include <algorithm>

using ClipConstHolder = std::shared_ptr<const ClipInterface>;

AudioSegmentFactory::AudioSegmentFactory(
    int sampleRate, int numChannels, ClipConstHolders clips)
    : mClips{std::move(clips)}
    , mSampleRate{sampleRate}
    , mNumChannels{numChannels}
{
}

std::vector<std::shared_ptr<AudioSegment> >
AudioSegmentFactory::CreateAudioSegmentSequence(
    double playbackStartTime, PlaybackDirection direction)
{
    return direction == PlaybackDirection::forward
           ? CreateAudioSegmentSequenceForward(playbackStartTime)
           : CreateAudioSegmentSequenceBackward(playbackStartTime);
}

std::vector<std::shared_ptr<AudioSegment> >
AudioSegmentFactory::CreateAudioSegmentSequenceForward(double t0)
{
    auto sortedClips = mClips;
    std::sort(
        sortedClips.begin(), sortedClips.end(),
        [](const std::shared_ptr<const ClipInterface>& a,
           const std::shared_ptr<const ClipInterface>& b) {
        return a->GetPlayStartTime() < b->GetPlayStartTime();
    });
    std::vector<std::shared_ptr<AudioSegment> > segments;
    for (const auto& clip : sortedClips) {
        if (clip->GetPlayStartTime() > t0) {
            const auto numSamples
                =sampleCount { (clip->GetPlayStartTime() - t0) * mSampleRate + .5 };
            segments.push_back(
                std::make_shared<SilenceSegment>(mNumChannels, numSamples));
            t0 = clip->GetPlayStartTime();
        } else if (clip->GetPlayEndTime() <= t0) {
            continue;
        }
        segments.push_back(std::make_shared<ClipSegment>(
                               *clip, t0 - clip->GetPlayStartTime(), PlaybackDirection::forward));
        t0 = clip->GetPlayEndTime();
    }
    return segments;
}

std::vector<std::shared_ptr<AudioSegment> >
AudioSegmentFactory::CreateAudioSegmentSequenceBackward(double t0)
{
    auto sortedClips = mClips;
    std::sort(
        sortedClips.begin(), sortedClips.end(),
        [&](
            const std::shared_ptr<const ClipInterface>& a,
            const std::shared_ptr<const ClipInterface>& b) {
        return a->GetPlayEndTime() > b->GetPlayEndTime();
    });
    std::vector<std::shared_ptr<AudioSegment> > segments;
    for (const auto& clip : sortedClips) {
        if (clip->GetPlayEndTime() < t0) {
            const auto numSamples
                =sampleCount { (t0 - clip->GetPlayEndTime()) * mSampleRate + .5 };
            segments.push_back(
                std::make_shared<SilenceSegment>(mNumChannels, numSamples));
            t0 = clip->GetPlayEndTime();
        } else if (clip->GetPlayStartTime() >= t0) {
            continue;
        }
        segments.push_back(std::make_shared<ClipSegment>(
                               *clip, clip->GetPlayEndTime() - t0, PlaybackDirection::backward));
        t0 = clip->GetPlayStartTime();
    }
    return segments;
}
