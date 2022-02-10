/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

  Audacity: A Digital Audio Editor

  FrameStatistics.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <chrono>
#include <cstddef>
#include <functional>

#include "Observer.h"

class GRAPHICS_API FrameStatistics final
{
public:
   using Clock = std::chrono::high_resolution_clock;
   using Duration = Clock::duration;
   using Timepoint = Clock::time_point;

   enum class SectionID
   {
      TrackPanel,
      WaveformView,
      SpectrumView,

      Count
   };

   struct GRAPHICS_API UpdatePublisher : Observer::Publisher<SectionID>
   {
      void Invoke(SectionID id);
   };

   class GRAPHICS_API Stopwatch final
   {
   public:
      ~Stopwatch() noexcept;
   private:
      explicit Stopwatch(SectionID section) noexcept;

      SectionID mSection;
      Timepoint mStart;

      friend class FrameStatistics;
   };

   class GRAPHICS_API Section final
   {
   public:
      Duration GetLastDuration() const noexcept;
      Duration GetMinDuration() const noexcept;
      Duration GetMaxDuration() const noexcept;
      Duration GetAverageDuration() const noexcept;
      size_t   GetEventsCount() const noexcept;
   private:
      void AddEvent(Duration duration) noexcept;

      static constexpr size_t KERNEL_SIZE = 16;

      Duration mLastDuration {};
      Duration mMinDuration { std::numeric_limits<Duration::rep>::max() };
      Duration mMaxDuration { std::numeric_limits<Duration::rep>::min() };
      Duration mAvgDuration {};

      Duration mFilteringKernel[KERNEL_SIZE];
      size_t mNextIndex { 0 };
      size_t mKernelItems { 0 };

      size_t mEventsCount { 0 };

      friend class FrameStatistics;
   };

   static Stopwatch CreateStopwatch(SectionID section) noexcept;

   static const Section& GetSection(SectionID section) noexcept;

   static Observer::Subscription Subscribe(UpdatePublisher::Callback callback);
private:
   void AddEvent(SectionID section, Duration duration) noexcept;

   Section mSections[size_t(SectionID::Count)];

   UpdatePublisher mUpdatePublisher;
};
