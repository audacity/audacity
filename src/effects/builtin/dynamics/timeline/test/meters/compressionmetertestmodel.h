#pragma once

#include "abstractdynamicsmetertestmodel.h"

namespace au::effects {
class CompressionMeterTestModel : public AbstractDynamicsMeterTestModel
{
    Q_OBJECT

public:
    Direction direction() const override { return Direction::Downwards; }
};
} // namespace au::effects
