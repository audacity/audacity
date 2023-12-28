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

class StftFrameProvider
{
public:
   StftFrameProvider(const MirAudioReader& source);
   bool GetNextFrame(std::vector<float>& frame);
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
