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

AbstractDbMeterModel::AbstractDbMeterModel(MeterValueProvider::Direction direction, QObject* parent)
    : AbstractDynamicsEffectInstanceModel{parent},
    m_valueQueue{std::make_shared<LockFreeQueue<float> >(audioFramesPerTick)},
    m_meter{MeterValueProvider::Create(direction)}
{
}

void AbstractDbMeterModel::doInit()
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }
    if (m_meter->GetDirection() == MeterValueProvider::Direction::Downwards) {
        instance->SetCompressionGainDbQueue(m_valueQueue);
    } else {
        instance->SetOutputDbQueue(m_valueQueue);
    }
}

void AbstractDbMeterModel::reset()
{
    m_meter = MeterValueProvider::Create(m_meter->GetDirection());
    emit valueChanged();
}

void AbstractDbMeterModel::update()
{
    constexpr auto updateFiveSecondMax = true;
    m_meter->Update(latestValue(), updateFiveSecondMax);

    emit valueChanged();
}

double AbstractDbMeterModel::currentMax() const { return m_meter->GetCurrentMax(); }

double AbstractDbMeterModel::globalMax() const { return m_meter->GetGlobalMax(); }

double AbstractDbMeterModel::fiveSecMax() const { return m_meter->GetFiveSecMax(); }

Stopwatch::PlayState AbstractDbMeterModel::playState() const
{
    return m_playState;
}

void AbstractDbMeterModel::setPlaystate(Stopwatch::PlayState state)
{
    if (m_playState == state) {
        return;
    }

    if (state == Stopwatch::Playing) {
        m_meter = MeterValueProvider::Create(m_meter->GetDirection());
        emit valueChanged();
    }

    m_playState = state;
    emit playStateChanged();
}
} // namespace au::effects
