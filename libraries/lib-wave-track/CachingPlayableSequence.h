/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file CachingPlayableSequence.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "AudioIOSequences.h"
#include "AudioSegmentSampleView.h"

#include <memory>

class WaveTrack;
using ChannelSampleView = std::vector<AudioSegmentSampleView>;

/** @brief A `PlayableSequence` decorator of `WaveTrack` that allows repeated
 * calls to `Get` mostly without fetching the samples on disk.
 *
 * A new fetch only happens when the new `Get` does not overlap the underlying
 * sample block (up to 1MB of audio) of the previous `Get`. Cache misses are
 * thus rare for continuous forward or backward sample readout.
 */
class WAVE_TRACK_API CachingPlayableSequence final : public PlayableSequence
{
public:
   CachingPlayableSequence(const WaveTrack&);

   // WideSampleSequence
   size_t NChannels() const override;
   float GetChannelGain(int channel) const override;
   bool Get(
      size_t iChannel, size_t nBuffers, samplePtr buffers[],
      sampleFormat format, sampleCount start, size_t len,
      fillFormat fill = fillZero, bool mayThrow = true,
      sampleCount* pNumWithinClips = nullptr) const override;
   double GetStartTime() const override;
   double GetEndTime() const override;
   double GetRate() const override;
   sampleFormat WidestEffectiveFormat() const override;
   bool HasTrivialEnvelope() const override;
   void GetEnvelopeValues(
      double* buffer, size_t bufferLen, double t0) const override;

   AudioGraph::ChannelType GetChannelType() const override;

   // PlayableSequence
   bool IsLeader() const override;
   bool GetSolo() const override;
   bool GetMute() const override;

private:
   const WaveTrack& mWaveTrack;
   // Ok to have it mutable so long as it is used by one thread only.
   // One per channel.
   mutable std::vector<ChannelSampleView> mCacheHolders;
};
