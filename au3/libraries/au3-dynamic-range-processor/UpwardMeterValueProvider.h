/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  UpwardMeterValueProvider.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include "DownwardMeterValueProvider.h"

class DYNAMIC_RANGE_PROCESSOR_API UpwardMeterValueProvider final : public MeterValueProvider
{
public:
    UpwardMeterValueProvider();

    void Update(float value, bool alsoFiveSecondMax) override;
    float GetGlobalMax() const override;
    float GetFiveSecMax() const override;
    float GetCurrentMax() const override;
    Direction GetDirection() const override;
    bool IsInvisible() const override;

private:
    DownwardMeterValueProvider mDownwardProvider;
};
