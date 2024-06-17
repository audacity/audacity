/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DownwardMeterValueProvider.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DownwardMeterValueProvider.h"
#include <algorithm>
#include <cassert>

namespace
{
constexpr auto decayPerSecondDb = 10.f;
constexpr auto decayPerTickDb =
   decayPerSecondDb * compressorMeterUpdatePeriodMs / 1000.f;
constexpr auto maxDelayMs = 5000;
} // namespace

DownwardMeterValueProvider::DownwardMeterValueProvider(float upperValue)
    : mUpperValue { upperValue }
    , mGlobalMin { upperValue }
    , mCurrentMin { upperValue }
    , mFiveSecMinState { upperValue }
{
   mRingBuffer.fill(upperValue);
}

void DownwardMeterValueProvider::Update(float newValue)
{
   ++mTimerCount;

   const auto value = mRingBuffer[mRingBufferIndex];
   mRingBuffer[mRingBufferIndex] = newValue;
   mRingBufferIndex = (mRingBufferIndex + 1) % ringBufferLength;

   if (value < mCurrentMin)
   {
      mCurrentMin = value;
      mLastFiveSeconds.emplace_back(mTimerCount, value);
      mGlobalMin = std::min<double>(mGlobalMin, value);
   }
   while (!mLastFiveSeconds.empty() &&
          mLastFiveSeconds.front().first <
             mTimerCount - maxDelayMs / compressorMeterUpdatePeriodMs)
      mLastFiveSeconds.erase(mLastFiveSeconds.begin());

   constexpr auto decayPerSecondDb = 10.f;
   if (value > mCurrentMin)
      mCurrentMin = std::min(mCurrentMin + decayPerTickDb, mUpperValue);

   if (!mLastFiveSeconds.empty())
   {
      const auto rawMin =
         std::min_element(
            mLastFiveSeconds.begin(), mLastFiveSeconds.end(),
            [](const auto& a, const auto& b) { return a.second < b.second; })
            ->second;
      if (rawMin < mFiveSecMinState)
         mFiveSecMinState = rawMin;
      else
         mFiveSecMinState =
            mFiveSecMinState * (1 - decayPerTickDb) + rawMin * decayPerTickDb;
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
