/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipInterface.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "SampleCount.h"
#include "SampleFormat.h"

class AudioSegmentSampleView;

class STRETCHING_SEQUENCE_API ClipTimes
{
public:
   virtual ~ClipTimes();

   /*!
    * The number of raw audio samples not hidden by trimming.
    */
   virtual sampleCount GetVisibleSampleCount() const = 0;

   virtual int GetRate() const = 0;

   virtual double GetPlayStartTime() const = 0;

   virtual double GetPlayEndTime() const = 0;

   virtual sampleCount TimeToSamples(double time) const = 0;

   virtual double GetStretchRatio() const = 0;
};

class STRETCHING_SEQUENCE_API ClipInterface : public ClipTimes
{
public:
   ~ClipInterface() override;

   virtual AudioSegmentSampleView
   GetSampleView(size_t iChannel, sampleCount start, size_t length,
      bool mayThrow = true) const = 0;

   virtual size_t GetWidth() const = 0;
};

using ClipHolders = std::vector<std::shared_ptr<ClipInterface>>;
using ClipConstHolders = std::vector<std::shared_ptr<const ClipInterface>>;
