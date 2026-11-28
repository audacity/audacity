/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchFactory.h

**********************************************************************/
#pragma once

#include "StretchMethod.h"
#include "TimeAndPitchInterface.h"

#include <memory>

/*!
 * The stretcher for a clip.
 *
 * `method` only decides how the change of speed is done. The pitch shift and
 * formant preservation are always StaffPad's, applied after it. A clip that
 * isn't stretched sounds the same whatever its method.
 *
 * @param numInputSamples how many samples `source` has left to give before it
 * runs out; SBSMS needs to know.
 */
TIME_AND_PITCH_API std::unique_ptr<TimeAndPitchInterface> CreateTimeAndPitchStretcher(
    StretchMethod method, int sampleRate, size_t numChannels, TimeAndPitchSource& source,
    const TimeAndPitchInterface::Parameters& parameters, long long numInputSamples);
