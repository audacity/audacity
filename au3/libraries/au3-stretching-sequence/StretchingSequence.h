/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StretchingSequence.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioIOSequences.h"
#include "PlaybackDirection.h"

#include <memory>
#include <optional>
#include <vector>

class AudioSegment;
class AudioSegmentFactoryInterface;
class ClipInterface;
using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface> >;

// For now this class assumes forward reading, which will be sufficient for the
// first goal of allowing export and rendering.
class STRETCHING_SEQUENCE_API StretchingSequence final : public PlayableSequence
{
public:
    static std::shared_ptr<StretchingSequence>
    Create(const PlayableSequence&, const ClipConstHolders& clips);

    StretchingSequence(
        const PlayableSequence&, int sampleRate, size_t numChannels, std::unique_ptr<AudioSegmentFactoryInterface>);

    // WideSampleSequence
    size_t NChannels() const override;
    float GetChannelVolume(int channel) const override;
    double GetStartTime() const override;
    double GetEndTime() const override;
    double GetRate() const override;
    sampleFormat WidestEffectiveFormat() const override;
    bool HasTrivialEnvelope() const override;
    void GetEnvelopeValues(
        double* buffer, size_t bufferLen, double t0, bool backwards) const override;
    bool DoGet(
        size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backwards,
        fillFormat fill = FillFormat::fillZero, bool mayThrow = true, sampleCount* pNumWithinClips = nullptr) const override;

    // PlayableSequence
    const ChannelGroup* FindChannelGroup() const override;
    bool GetSolo() const override;
    bool GetMute() const override;

    // AudioGraph::Channel
    AudioGraph::ChannelType GetChannelType() const override;

    // class methods
    bool GetFloats(
        float* buffers[], sampleCount start, size_t len, bool backwards) const;

private:
    using AudioSegments = std::vector<std::shared_ptr<AudioSegment> >;

    void ResetCursor(double t, PlaybackDirection);
    bool GetNext(float* const buffers[], size_t numChannels, size_t numSamples);
    bool MutableGet(
        size_t iChannel, size_t nBuffers, const samplePtr buffers[], sampleFormat format, sampleCount start, size_t len, bool backwards);

    const PlayableSequence& mSequence;
    const std::unique_ptr<AudioSegmentFactoryInterface> mAudioSegmentFactory;
    AudioSegments mAudioSegments;
    AudioSegments::const_iterator mActiveAudioSegmentIt = mAudioSegments.end();
    std::optional<sampleCount> mExpectedStart;
    PlaybackDirection mPlaybackDirection = PlaybackDirection::forward;
};
