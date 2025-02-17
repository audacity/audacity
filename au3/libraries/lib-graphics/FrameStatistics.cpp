/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FrameStatistics.cpp

  Dmitry Vedenko

**********************************************************************/

#include "FrameStatistics.h"

#include <algorithm>
#include <numeric>

namespace {
FrameStatistics& GetInstance() noexcept
{
    static FrameStatistics frameStatistics;
    return frameStatistics;
}
}

FrameStatistics::Stopwatch::~Stopwatch() noexcept
{
    GetInstance().AddEvent(mSection, FrameStatistics::Clock::now() - mStart);
}

FrameStatistics::Stopwatch::Stopwatch(SectionID section) noexcept
    : mSection(section)
    , mStart(FrameStatistics::Clock::now())
{
}

FrameStatistics::Duration
FrameStatistics::Section::GetLastDuration() const noexcept
{
    return mLastDuration;
}

FrameStatistics::Duration
FrameStatistics::Section::GetMinDuration() const noexcept
{
    return mMinDuration;
}

FrameStatistics::Duration
FrameStatistics::Section::GetMaxDuration() const noexcept
{
    return mMaxDuration;
}

FrameStatistics::Duration
FrameStatistics::Section::GetAverageDuration() const noexcept
{
    return mAvgDuration;
}

size_t FrameStatistics::Section::GetEventsCount() const noexcept
{
    return mEventsCount;
}

void FrameStatistics::Section::AddEvent(Duration duration) noexcept
{
    ++mEventsCount;

    mLastDuration = duration;

    mMinDuration = std::min(mMinDuration, duration);
    mMaxDuration = std::max(mMaxDuration, duration);

    // Kernel is initialized with zeroes
    mAvgAccum = mAvgAccum - mFilteringKernel[mNextIndex] + duration;

    mFilteringKernel[mNextIndex] = duration;

    mNextIndex = (mNextIndex + 1) % KERNEL_SIZE;

    if (mKernelItems < KERNEL_SIZE) {
        ++mKernelItems;
    }

    mAvgDuration = mAvgAccum / mKernelItems;
}

FrameStatistics::Stopwatch
FrameStatistics::CreateStopwatch(SectionID section) noexcept
{
    // New frame has started
    if (section == SectionID::TrackPanel) {
        auto& instance = GetInstance();

        instance.mSections[size_t(SectionID::WaveformView)] = {};
        instance.mSections[size_t(SectionID::WaveDataCache)] = {};
        instance.mSections[size_t(SectionID::WaveBitmapCachePreprocess)] = {};
        instance.mSections[size_t(SectionID::WaveBitmapCache)] = {};
    }

    return Stopwatch(section);
}

const FrameStatistics::Section&
FrameStatistics::GetSection(SectionID section) noexcept
{
    if (section < SectionID::Count) {
        return GetInstance().mSections[size_t(section)];
    }

    static Section fakeSection;
    return fakeSection;
}

Observer::Subscription
FrameStatistics::Subscribe(UpdatePublisher::Callback callback)
{
    return GetInstance().mUpdatePublisher.Subscribe(std::move(callback));
}

void FrameStatistics::AddEvent(SectionID section, Duration duration)
{
    if (section < SectionID::Count) {
        GetInstance().mSections[size_t(section)].AddEvent(duration);
        GetInstance().mUpdatePublisher.Invoke(section);
    }
}

void FrameStatistics::UpdatePublisher::Invoke(FrameStatistics::SectionID id)
{
    Publish(id);
}
