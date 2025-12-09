/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UpwardMeterValueProvider.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "UpwardMeterValueProvider.h"

UpwardMeterValueProvider::UpwardMeterValueProvider()
    : mDownwardProvider{80.f}
{
}

void UpwardMeterValueProvider::Update(float value, bool alsoFiveSecondMax)
{
    mDownwardProvider.Update(-value, alsoFiveSecondMax);
}

float UpwardMeterValueProvider::GetGlobalMax() const
{
    return -mDownwardProvider.GetGlobalMax();
}

float UpwardMeterValueProvider::GetFiveSecMax() const
{
    return -mDownwardProvider.GetFiveSecMax();
}

float UpwardMeterValueProvider::GetCurrentMax() const
{
    return -mDownwardProvider.GetCurrentMax();
}

MeterValueProvider::Direction UpwardMeterValueProvider::GetDirection() const
{
    return Direction::Upwards;
}

bool UpwardMeterValueProvider::IsInvisible() const
{
    return mDownwardProvider.IsInvisible();
}
