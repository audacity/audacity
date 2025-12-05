/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TimeAndPitchFakeSource.cpp

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "TimeAndPitchInterface.h"

struct TimeAndPitchFakeSource final : public TimeAndPitchSource
{
    void Pull(float* const* buffer, size_t numSamples) override
    {
    }
};
