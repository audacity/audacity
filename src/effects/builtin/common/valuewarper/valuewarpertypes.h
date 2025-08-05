/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

namespace au::effects {
class ValueWarpingTypes
{
    Q_GADGET
public:
    enum class ValueWarpingType {
        None,
        Soft,
        Aggressive,
    };
    Q_ENUM(ValueWarpingType)
};

using ValueWarpingType = ValueWarpingTypes::ValueWarpingType;

class IValueTransformer
{
public:
    virtual ~IValueTransformer() = default;

    virtual double forward(double x, double min, double max) const = 0;
    virtual double inverse(double y, double min, double max) const = 0;
};
} // namespace au::effects
