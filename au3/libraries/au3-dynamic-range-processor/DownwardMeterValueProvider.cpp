/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DownwardMeterValueProvider.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DownwardMeterValueProvider.h"
#include <algorithm>
#include <cassert>

namespace {
constexpr auto decayPerSecondDb = 10.f;
constexpr auto decayPerTickDb
    =decayPerSecondDb * compressorMeterUpdatePeriodMs / 1000.f;
constexpr auto maxDelayMs = 5000;
constexpr auto maxTickCount = maxDelayMs / compressorMeterUpdatePeriodMs;
} // namespace

DownwardMeterValueProvider::DownwardMeterValueProvider(float upperValue)
    : mUpperValue{upperValue}
    , mGlobalMin{upperValue}
    , mCurrentMin{upperValue}
    , mFiveSecMinState{upperValue}
{
    mRingBuffer.fill(upperValue);
}

void DownwardMeterValueProvider::Update(float newValue, bool alsoFiveSecondMax)
{
    ++mTimerCount;

    const auto value = mRingBuffer[mRingBufferIndex];
    mRingBuffer[mRingBufferIndex] = newValue;
    mRingBufferIndex = (mRingBufferIndex + 1) % ringBufferLength;

    if (value < mCurrentMin) {
        mCurrentMin = value;
        mGlobalMin = std::min(mGlobalMin, value);
    } else {
        mCurrentMin = std::min(mCurrentMin + decayPerTickDb, mUpperValue);
    }

    mLastFiveSeconds.emplace_back(mTimerCount, value);
    while (!mLastFiveSeconds.empty()
           && mLastFiveSeconds.front().first < mTimerCount - maxTickCount) {
        mLastFiveSeconds.erase(mLastFiveSeconds.begin());
    }

    if (!mLastFiveSeconds.empty() && alsoFiveSecondMax) {
        const auto rawMin
            =std::min_element(
                  mLastFiveSeconds.begin(), mLastFiveSeconds.end(),
                  [](const auto& a, const auto& b) { return a.second < b.second; })
              ->second;
        if (rawMin <= mFiveSecMinState) {
            mFiveSecMinState = rawMin;
        } else {
            mFiveSecMinState
                =std::min(mFiveSecMinState + decayPerTickDb, mUpperValue);
        }
    }
}

float DownwardMeterValueProvider::GetGlobalMax() const
{
    return mGlobalMin;
}

float DownwardMeterValueProvider::GetFiveSecMax() const
{
    return mFiveSecMinState;
}

float DownwardMeterValueProvider::GetCurrentMax() const
{
    return mCurrentMin;
}

DownwardMeterValueProvider::Direction
DownwardMeterValueProvider::GetDirection() const
{
    return Direction::Downwards;
}

bool DownwardMeterValueProvider::IsInvisible() const
{
    return mCurrentMin >= mUpperValue;
}
