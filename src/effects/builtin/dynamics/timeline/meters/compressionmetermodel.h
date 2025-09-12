#pragma once

#include "abstractdynamicsmetermodel.h"

namespace au::effects {
class CompressionMeterModel : public AbstractDynamicsMeterModel
{
    Q_OBJECT

public:
    Direction direction() const override { return Direction::Downwards; }
};
} // namespace au::effects
