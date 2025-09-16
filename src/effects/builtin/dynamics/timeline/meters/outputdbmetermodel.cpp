/*
 * Audacity: A Digital Audio Editor
 */
#include "outputdbmetermodel.h"

#include "global/log.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"

namespace au::effects {
OutputDbMeterModel::OutputDbMeterModel(QObject* parent)
    : AbstractDbMeterModel{-100.0, parent}
{
}

void OutputDbMeterModel::doInit()
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }
    m_meter = MeterValueProvider::Create(MeterValueProvider::Direction::Upwards);
    instance->SetOutputDbQueue(m_valueQueue);
}

float OutputDbMeterModel::latestValue()
{
    auto highestOutputGain = std::numeric_limits<float>::lowest();
    auto tmp = 0.f;
    while (m_valueQueue->Get(tmp)) {
        highestOutputGain = std::max(highestOutputGain, tmp);
    }
    return highestOutputGain;
}
}
