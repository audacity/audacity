/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchInterface.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <vector>

using PitchRatioChangeCbSubscriber =
   std::function<void(std::function<void(double)>)>;

class TIME_AND_PITCH_API TimeAndPitchSource
{
public:
   virtual ~TimeAndPitchSource();
   virtual void Pull(float* const*, size_t samplesPerChannel) = 0;
};

class TIME_AND_PITCH_API TimeAndPitchInterface
{
public:
   static bool IsPassThroughMode(double stretchRatio);

   struct Parameters
   {
      std::optional<double> timeRatio;
      std::optional<double> pitchRatio;
      PitchRatioChangeCbSubscriber pitchRatioChangeCbSubscriber;
   };

   virtual void GetSamples(float* const*, size_t) = 0;

   virtual ~TimeAndPitchInterface();
};

class TIME_AND_PITCH_API PitchShiftChangePublisher
{
public:
   virtual ~PitchShiftChangePublisher();
   virtual void Publish(double semitones) = 0;
};
