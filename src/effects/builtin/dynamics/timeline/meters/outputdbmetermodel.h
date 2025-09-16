/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "abstractdbmetermodel.h"

namespace au::effects {
class OutputDbMeterModel : public AbstractDbMeterModel
{
    Q_OBJECT

public:
    explicit OutputDbMeterModel(QObject* parent = nullptr);

    void doInit() override;
    float latestValue() override;
};
} // namespace au::effects
