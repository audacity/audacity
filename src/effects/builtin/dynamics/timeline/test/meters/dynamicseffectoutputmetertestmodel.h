/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractdynamicsmetertestmodel.h"

namespace au::effects {
class DynamicsEffectOutputMeterTestModel : public AbstractDynamicsMeterTestModel
{
    Q_OBJECT

public:
    Direction direction() const override { return Direction::Upwards; }
};
} // namespace au::effects
