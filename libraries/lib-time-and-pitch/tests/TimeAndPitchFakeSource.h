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
   size_t Pull(float* const* buffer, size_t, size_t) override
   {
      return 0u;
   }
   bool Empty() const override
   {
      return empty;
   }
   bool empty = false;
};
