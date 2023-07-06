/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveClipUtilities.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVE_CLIP_UTILITIES__
#define __AUDACITY_WAVE_CLIP_UTILITIES__

#include <cstddef>
#include <vector>

class sampleCount;

AUDACITY_DLL_API
void findCorrection(
   const std::vector<sampleCount>& oldWhere, size_t oldLen, size_t newLen,
   double t0, double sampleRate, double stretchRatio, double samplesPerPixel,
   int& oldX0, double& correction);

AUDACITY_DLL_API
void fillWhere(
   std::vector<sampleCount>& where, size_t len, bool addBias, double correction,
   double t0, double sampleRate, double stretchRatio, double samplesPerPixel);

#endif
