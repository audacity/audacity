/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <QObject>

namespace au::effects {
class IValueTransformer
{
public:
    virtual ~IValueTransformer() = default;

    virtual double forward(double x) const = 0;
    virtual double inverse(double y) const = 0;
};
} // namespace au::effects
