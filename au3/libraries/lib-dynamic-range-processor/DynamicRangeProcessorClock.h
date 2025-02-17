/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  DynamicRangeProcessorClock.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <chrono>
#include <optional>

/*!
 * \brief A clock that can be paused and resumed.
 */
class DYNAMIC_RANGE_PROCESSOR_API DynamicRangeProcessorClock final
{
public:
    std::chrono::steady_clock::time_point GetNow() const;
    void Pause();
    void Resume();

private:
    std::optional<std::chrono::steady_clock::time_point> mPauseBegin;
    std::chrono::duration<double> mElapsedWhilePaused
        =std::chrono::duration<double>::zero();
};
