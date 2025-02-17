/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FrameStatistics.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <chrono>
#include <cstddef>
#include <functional>

#include "Observer.h"

//! A class to profile TrackPanel painting
/*!
 * Object of this class is a global singleton. If there are multiple
 * opened projects, the statistics will be merged.
 */
class GRAPHICS_API FrameStatistics final
{
public:
    using Clock = std::chrono::high_resolution_clock;
    using Duration = Clock::duration;
    using Timepoint = Clock::time_point;

    //! ID of the profiling section
    enum class SectionID
    {
        //! Full repaint time of the TrackPanel
        TrackPanel,
        //! Time required to draw a single clip
        WaveformView,
        //! Time required to access the data cache
        WaveDataCache,
        //! Time required to build the structures required for the bitmap cache population
        WaveBitmapCachePreprocess,
        //! Time required to access the wave bitmaps cache
        WaveBitmapCache,
        //! Number of the sections
        Count
    };

    //! A helper that notifies the view that a specific section has changed
    struct GRAPHICS_API UpdatePublisher : Observer::Publisher<SectionID>
    {
        void Invoke(SectionID id);
    };

    //! RAII wrapper used to measure a section time
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

    //! Profiling section data
    class GRAPHICS_API Section final
    {
    public:
        //! Duration of the last event
        Duration GetLastDuration() const noexcept;
        //! All time minimum duration of the events in this section
        Duration GetMinDuration() const noexcept;
        //! All time maximum duration of the events in this section
        Duration GetMaxDuration() const noexcept;
        //! Average duration of the last KERNEL_SIZE events in this section
        Duration GetAverageDuration() const noexcept;
        //! Total number of the events in this section
        size_t   GetEventsCount() const noexcept;
    private:
        void AddEvent(Duration duration) noexcept;

        static constexpr size_t KERNEL_SIZE = 16;

        Duration mLastDuration {};
        Duration mMinDuration { std::numeric_limits<Duration::rep>::max() };
        Duration mMaxDuration { std::numeric_limits<Duration::rep>::min() };
        Duration mAvgAccum {};
        Duration mAvgDuration {};

        Duration mFilteringKernel[KERNEL_SIZE] {};
        size_t mNextIndex { 0 };
        size_t mKernelItems { 0 };

        size_t mEventsCount { 0 };

        friend class FrameStatistics;
    };

    //! Create a Stopwatch for the section specified
    static Stopwatch CreateStopwatch(SectionID section) noexcept;
    //! Get the section data
    static const Section& GetSection(SectionID section) noexcept;
    //! Subscribe to sections update
    static Observer::Subscription Subscribe(UpdatePublisher::Callback callback);
private:
    void AddEvent(SectionID section, Duration duration);

    Section mSections[size_t(SectionID::Count)];

    UpdatePublisher mUpdatePublisher;
};
