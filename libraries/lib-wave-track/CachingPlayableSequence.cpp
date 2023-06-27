/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CachingPlayableSequence.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "CachingPlayableSequence.h"
#include "AudioSegmentSampleView.h"
#include "WaveTrack.h"

#include <cassert>

namespace
{
void FillBufferFromTrackBlockSequence(
   const ChannelSampleView& track, float* buffer, size_t bufferSize)
{
   size_t written = 0;
   for (const auto& segment : track)
   {
      size_t toWrite =
         limitSampleBufferSize(bufferSize - written, segment.GetSampleCount());
      written += segment.Copy(buffer + written, toWrite);
   }
   std::fill(buffer + written, buffer + bufferSize, 0.f);
}
} // namespace

CachingPlayableSequence::CachingPlayableSequence(const WaveTrack& waveTrack)
    : mWaveTrack { waveTrack }
{
}

size_t CachingPlayableSequence::NChannels() const
{
   return mWaveTrack.NChannels();
}

float CachingPlayableSequence::GetChannelGain(int channel) const
{
   return mWaveTrack.GetChannelGain(channel);
}

bool CachingPlayableSequence::Get(
   size_t iChannel, size_t nBuffers, samplePtr buffers[], sampleFormat format,
   sampleCount start, size_t len, fillFormat fill, bool mayThrow,
   sampleCount* pNumWithinClips) const
{
   assert(iChannel + nBuffers <= mWaveTrack.NChannels());
   mCacheHolders = mWaveTrack.GetSampleView(iChannel, nBuffers, start, len);
   assert(mCacheHolders.size() == nBuffers);
   for (auto i = 0u; i < mCacheHolders.size(); ++i)
      FillBufferFromTrackBlockSequence(
         mCacheHolders[i], reinterpret_cast<float*>(buffers[i]), len);
   return !mCacheHolders.empty();
}

double CachingPlayableSequence::GetStartTime() const
{
   return mWaveTrack.GetStartTime();
}

double CachingPlayableSequence::GetEndTime() const
{
   return mWaveTrack.GetEndTime();
}

double CachingPlayableSequence::GetRate() const
{
   return mWaveTrack.GetRate();
}

sampleFormat CachingPlayableSequence::WidestEffectiveFormat() const
{
   return mWaveTrack.WidestEffectiveFormat();
}

bool CachingPlayableSequence::HasTrivialEnvelope() const
{
   return mWaveTrack.HasTrivialEnvelope();
}

void CachingPlayableSequence::GetEnvelopeValues(
   double* buffer, size_t bufferLen, double t0) const
{
   mWaveTrack.GetEnvelopeValues(buffer, bufferLen, t0);
}

AudioGraph::ChannelType CachingPlayableSequence::GetChannelType() const
{
   return mWaveTrack.GetChannelType();
}

bool CachingPlayableSequence::IsLeader() const
{
   return mWaveTrack.IsLeader();
}

bool CachingPlayableSequence::GetSolo() const
{
   return mWaveTrack.GetSolo();
}

bool CachingPlayableSequence::GetMute() const
{
   return mWaveTrack.GetMute();
}
