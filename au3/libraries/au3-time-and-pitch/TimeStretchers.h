/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeStretchers.h

**********************************************************************/
#pragma once

#include "TimeAndPitchInterface.h"

#include <cstddef>
#include <memory>

/*!
 * Speed-only stretchers, private to this library.
 *
 * Each pulls from `upstream` and gives it back slower or faster: `timeRatio`
 * is output duration over input duration. Each is itself a
 * `TimeAndPitchSource`, so that StaffPad can shift its pitch afterwards. Once
 * `upstream` runs out it gives silence, and so do they.
 *
 * Callers handle `timeRatio == 1`; these always stretch.
 */
namespace TimeStretchers {
std::unique_ptr<TimeAndPitchSource> CreateVarispeed(
    size_t numChannels, TimeAndPitchSource& upstream, double timeRatio);

std::unique_ptr<TimeAndPitchSource> CreatePaulstretch(
    int sampleRate, size_t numChannels, TimeAndPitchSource& upstream, double timeRatio);

//! nullptr if this build has no SoundTouch
std::unique_ptr<TimeAndPitchSource> CreateSoundTouch(
    int sampleRate, size_t numChannels, TimeAndPitchSource& upstream, double timeRatio);

//! nullptr if this build has no SBSMS
std::unique_ptr<TimeAndPitchSource> CreateSbsms(
    size_t numChannels, TimeAndPitchSource& upstream, double timeRatio, long long numInputSamples);
}
