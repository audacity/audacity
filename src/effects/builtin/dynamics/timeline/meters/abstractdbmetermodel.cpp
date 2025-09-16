/*
 * Audacity: A Digital Audio Editor
 */
#include "abstractdbmetermodel.h"
#include "playback/iplayer.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "libraries/lib-dynamic-range-processor/MeterValueProvider.h"

namespace au::effects {
namespace {
// An overestimate: 44.1kHz and 512 samples per buffer, in frames per second,
// would be 86.13.
constexpr auto audioFramePerSec = 200;
constexpr auto ticksPerSec = 1000 / compressorMeterUpdatePeriodMs;
// This will be set as max size for the lock-free queue.
constexpr auto audioFramesPerTick = audioFramePerSec / ticksPerSec;
} // namespace

AbstractDbMeterModel::AbstractDbMeterModel(double defaultValue, QObject* parent)
    : AbstractDynamicsEffectInstanceModel{parent}, m_valueQueue{std::make_shared<LockFreeQueue<float> >(audioFramesPerTick)},
    m_currentMax{defaultValue}, m_globalMax{defaultValue}, m_fiveSecMax{defaultValue}
{
}

void AbstractDbMeterModel::update()
{
    constexpr auto updateFiveSecondMax = true;
    m_meter->Update(latestValue(), updateFiveSecondMax);

    m_currentMax = m_meter->GetCurrentMax();
    m_globalMax = m_meter->GetGlobalMax();
    m_fiveSecMax = m_meter->GetFiveSecMax();

    emit valueChanged();
}

double AbstractDbMeterModel::currentMax() const { return m_currentMax; }

double AbstractDbMeterModel::globalMax() const { return m_globalMax; }

double AbstractDbMeterModel::fiveSecMax() const { return m_fiveSecMax; }
} // namespace au::effects
