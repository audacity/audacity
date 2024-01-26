/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  StftFrameProvider.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <vector>

namespace MIR
{
class MirAudioReader;

/*!
 * Utility class to provide time-domain frames ready for FFT. The returned
 * frames have power-of-two size and are already windowed with a Hann window,
 * scaled such that it sums to unity. Also, `GetNumFrames()` is the closest
 * power of two that satifsies a hop size of 10ms. This property facilitates the
 * FFT analysis of transformation of the STFT frames to some scalar, e.g. the
 * novelty values of an onset detection function.
 */
class MUSIC_INFORMATION_RETRIEVAL_API StftFrameProvider
{
public:
   StftFrameProvider(const MirAudioReader& source);
   StftFrameProvider(StftFrameProvider&&) = default;
   
   bool GetNextFrame(std::vector<float>& frame);
   void SkipFrames(int numFrames);
   int GetNumFrames() const;
   int GetSampleRate() const;
   double GetFrameRate() const;
   int GetFftSize() const;

private:
   const MirAudioReader& mAudio;
   const int mFftSize;
   const double mHopSize;
   const std::vector<float> mWindow;
   const int mNumFrames;
   const long long mNumSamples;
   int mNumFramesProvided = 0;
};
} // namespace MIR
