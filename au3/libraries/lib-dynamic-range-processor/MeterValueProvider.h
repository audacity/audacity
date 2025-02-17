/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  MeterValueProvider.h

  Matthieu Hodgkinson

**********************************************************************/
#pragma once

#include <memory>

class DYNAMIC_RANGE_PROCESSOR_API MeterValueProvider
{
public:
    enum class Direction
    {
        Upwards,
        Downwards
    };

    static std::unique_ptr<MeterValueProvider> Create(Direction direction);

    virtual ~MeterValueProvider() = default;
    virtual void Update(float value, bool alsoFiveSecondMax) = 0;
    virtual float GetGlobalMax() const = 0;
    virtual float GetFiveSecMax() const = 0;
    virtual float GetCurrentMax() const = 0;
    virtual Direction GetDirection() const = 0;
    virtual bool IsInvisible() const = 0;
};
