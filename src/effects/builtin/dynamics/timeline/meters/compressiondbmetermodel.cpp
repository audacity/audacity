/*
 * Audacity: A Digital Audio Editor
 */
#include "compressiondbmetermodel.h"

#include "global/log.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"

namespace au::effects {
CompressionDbMeterModel::CompressionDbMeterModel(QObject* parent)
    : AbstractDbMeterModel{MeterValueProvider::Direction::Downwards, parent}
{
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
