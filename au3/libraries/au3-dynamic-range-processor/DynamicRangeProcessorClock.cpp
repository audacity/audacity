/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorClock.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "DynamicRangeProcessorClock.h"

std::chrono::steady_clock::time_point DynamicRangeProcessorClock::GetNow() const
{
    return std::chrono::steady_clock::now()
           - std::chrono::duration_cast<std::chrono::steady_clock::duration>(
        mElapsedWhilePaused);
}

void DynamicRangeProcessorClock::Pause()
{
    mPauseBegin = std::chrono::steady_clock::now();
}

void DynamicRangeProcessorClock::Resume()
{
    if (!mPauseBegin.has_value()) {
        return;
    }
    mElapsedWhilePaused += std::chrono::steady_clock::now() - *mPauseBegin;
}
