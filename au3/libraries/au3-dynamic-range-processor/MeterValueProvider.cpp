/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MeterValueProvider.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "MeterValueProvider.h"
#include "DownwardMeterValueProvider.h"
#include "UpwardMeterValueProvider.h"

#include <cassert>

std::unique_ptr<MeterValueProvider>
MeterValueProvider::Create(Direction direction)
{
    switch (direction) {
    case Direction::Upwards:
        return std::make_unique<UpwardMeterValueProvider>();
    case Direction::Downwards:
        return std::make_unique<DownwardMeterValueProvider>();
    default:
        assert(false);
        return nullptr;
    }
}
