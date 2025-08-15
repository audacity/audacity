/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

namespace au::effects {
class ControlWarpingTypes
{
    Q_GADGET
public:
    enum class ControlWarpingType {
        None,
        Soft,
        Aggressive,
    };
    Q_ENUM(ControlWarpingType)
};

using ControlWarpingType = ControlWarpingTypes::ControlWarpingType;

class IValueTransformer
{
public:
    virtual ~IValueTransformer() = default;

    virtual double forward(double x, double min, double max) const = 0;
    virtual double inverse(double y, double min, double max) const = 0;
};
} // namespace au::effects
