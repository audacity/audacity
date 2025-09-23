/*
 * Audacity: A Digital Audio Editor
 */
#include "outputdbmetermodel.h"

#include "global/log.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"

namespace au::effects {
OutputDbMeterModel::OutputDbMeterModel(QObject* parent)
    : AbstractDbMeterModel{MeterValueProvider::Direction::Upwards, parent}
{
}

float OutputDbMeterModel::latestValue()
{
    auto highestOutputGain = std::numeric_limits<float>::lowest();
    auto tmp = 0.f;
    while (m_valueQueue->Get(tmp)) {
        highestOutputGain = std::max(highestOutputGain, tmp);
    }
    if (!m_isClipping && highestOutputGain > 0.f) {
        m_isClipping = true;
        emit isClippingChanged();
    }
    return highestOutputGain;
}

void OutputDbMeterModel::setIsClipping(bool isClipping)
{
    if (m_isClipping == isClipping) {
        return;
    }

    m_isClipping = isClipping;
    emit isClippingChanged();
}
}
