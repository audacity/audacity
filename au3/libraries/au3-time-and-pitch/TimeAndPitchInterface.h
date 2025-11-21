/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchInterface.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <functional>
#include <memory>
#include <optional>
#include <vector>

class TIME_AND_PITCH_API TimeAndPitchSource
{
public:
    virtual ~TimeAndPitchSource();
    virtual void Pull(float* const*, size_t samplesPerChannel) = 0;
};

class TIME_AND_PITCH_API TimeAndPitchInterface
{
public:
    static bool IsPassThroughMode(double stretchRatio);

    // Limit the pitch shift range to +/- one octave. When
    // https://github.com/audacity/audacity/issues/5904 is addressed, we may
    // alleviate this restriction.
    static constexpr auto MaxCents = 1200;
    static constexpr auto MinCents = -1200;

    struct Parameters
    {
        double timeRatio = 1.0;
        double pitchRatio = 1.0;
        bool preserveFormants = false;
    };

    virtual void GetSamples(float* const*, size_t) = 0;
    virtual void OnCentShiftChange(int cents) = 0;
    virtual void OnFormantPreservationChange(bool preserve) = 0;

    virtual ~TimeAndPitchInterface();
};
