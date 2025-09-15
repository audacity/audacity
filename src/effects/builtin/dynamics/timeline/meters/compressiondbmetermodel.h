#pragma once

#include "abstractdbmetermodel.h"

namespace au::effects {
class CompressionDbMeterModel : public AbstractDbMeterModel
{
    Q_OBJECT

public:
    void doInit() override;
    float latestValue() override;
};
} // namespace au::effects
