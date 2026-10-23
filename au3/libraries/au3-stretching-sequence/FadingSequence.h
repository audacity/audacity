/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FadingSequence.h

**********************************************************************/
#pragma once

#include "au3-mixer/AudioIOSequences.h"
#include "ClipFades.h"
#include "ClipInterface.h"

#include <memory>
#include <optional>
#include <vector>

//! Applies the clips' fades to the output of the sequence it decorates,
//! typically a `StretchingSequence`, i.e., after stretching.
/*!
 * Fades may be changed while the sequence is being read, e.g. during playback;
 * the clips' boundaries may not.
 */
class STRETCHING_SEQUENCE_API FadingSequence final : public PlayableSequence
{
public:
    //! Stretches, then fades `sequence`
    static std::shared_ptr<FadingSequence>
    Create(const PlayableSequence& sequence, const ClipConstHolders& clips);

    //! Must be constructed on the main thread
    FadingSequence(std::shared_ptr<const PlayableSequence> sequence, const ClipConstHolders& clips);

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

private:
    //! When the fades change during playback, the gain cross-fades from the previous ones to the new ones,
    //! so that it doesn't click. A change arriving during a ramp waits for the end of it.
    struct FadedClip {
        std::shared_ptr<const ClipInterface> clip;
        const ClipFades& fades;
        ClipFades::Snapshot from;
        ClipFades::Snapshot to;
        //! How far into the ramp from `from` to `to` we are, in samples
        size_t rampPosition = 0;
    };

    void ApplyFades(
        size_t nBuffers, const samplePtr buffers[], sampleCount start, size_t len, bool backwards) const;

    const std::shared_ptr<const PlayableSequence> mSequence;

    // Mutable: reading the sequence advances the ramps
    mutable std::vector<FadedClip> mClips;
    //! Where the next read continues the previous one; another read jumps to the latest fades
    mutable std::optional<sampleCount> mExpectedStart;
    mutable bool mExpectedBackwards = false;
};
