#pragma once

#include "abstractdynamicsmetermodel.h"

namespace au::effects {
class DynamicsEffectOutputMeterModel : public AbstractDynamicsMeterModel
{
    Q_OBJECT

public:
    Direction direction() const override { return Direction::Upwards; }
};
} // namespace au::effects
