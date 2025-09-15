/*
 * Audacity: A Digital Audio Editor
 */
#include "compressiondbmetermodel.h"

#include "global/log.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"

namespace au::effects {
void CompressionDbMeterModel::doInit()
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }
    instance->SetCompressionGainDbQueue(m_valueQueue);
    m_meter = MeterValueProvider::Create(MeterValueProvider::Direction::Downwards);
}

float CompressionDbMeterModel::latestValue()
{
    auto lowestCompressionGain = 0.f;
    auto tmp = 0.f;
    while (m_valueQueue->Get(tmp)) {
        lowestCompressionGain = std::min(lowestCompressionGain, tmp);
    }
    return lowestCompressionGain;
}
} // namespace au::effects
