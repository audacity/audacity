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

class TIME_AND_PITCH_API TimeAndPitchSource
{
public:
   virtual ~TimeAndPitchSource() = default;
   virtual size_t
   Pull(float* const*, size_t numChannels, size_t samplesPerChannel) = 0;
   virtual bool Empty() const = 0;
};

class TIME_AND_PITCH_API TimeAndPitchInterface
{
public:
   struct Parameters
   {
      std::optional<double> timeRatio;
      std::optional<double> pitchRatio;
   };

   virtual void GetSamples(float* const*, size_t) = 0;

   virtual bool CanReturnMoreSamples() const = 0;

   virtual ~TimeAndPitchInterface() = default;
};
