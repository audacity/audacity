#include "timelinesourcemodel.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorHistory.h"

#include "global/log.h"

#include <QLineSeries>
#include <QSGGeometryNode>
#include <QTimer>
#include <QtQuick/qsgflatcolormaterial.h>

namespace au::effects {
namespace {
constexpr auto deliveryPeriodMs = 50;
}

TimelineSourceModel::TimelineSourceModel(QObject* parent)
    : QObject(parent) {}

void TimelineSourceModel::init()
{
    m_deliveryTimer = new QTimer(this);
    connect(m_deliveryTimer, &QTimer::timeout, this, [this] { pullData(); });
    m_deliveryTimer->setInterval(deliveryPeriodMs);

    const auto instance = std::dynamic_pointer_cast<::CompressorInstance>(instancesRegister()->instanceById(m_instanceId));
    IF_ASSERT_FAILED(instance) {
        LOGW() << "Could not find instance for id " << m_instanceId;
        return;
    }
    m_instance = instance;

    mInitializeProcessingSettingsSubscription
        = static_cast<InitializeProcessingSettingsPublisher&>(*instance).Subscribe([&](const std::optional<InitializeProcessingSettings>&
                                                                                       evt) {
        if (evt) {
            initializeForPlayback(evt->sampleRate);
        } else {
            // Stop the timer-based update but keep the history until playback is resumed.
            m_deliveryTimer->stop();
        }
    });
}

double TimelineSourceModel::latency() const
{
    return deliveryPeriodMs / 1000.0;
}

double TimelineSourceModel::dataPointRate() const
{
    return 44100.0 / 512; // TODO query from the instance
}

void TimelineSourceModel::setInstanceId(int id)
{
    if (m_instanceId == id) {
        return;
    }
    m_instanceId = id;
    emit effectIdChanged();
}

void TimelineSourceModel::pullData()
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }

    mPacketBuffer.clear();
    ::DynamicRangeProcessorOutputPacket packet;
    while (mOutputQueue->Get(packet)) {
        mPacketBuffer.push_back(packet);
    }
    mHistory->Push(mPacketBuffer);

    const DynamicRangeProcessorHistory::PacketView view = mHistory->GetViewOnNewPackets();
    if (view.numPackets() == 0) {
        return;
    }

    QVariantList samples;
    samples.reserve(view.numPackets());
    for (auto i = 0; i < view.numPackets(); ++i) {
        const DynamicRangeProcessorHistory::Packet& p = view.at(i);
        LOGI() << "time: " << p.time;
        samples.append(QVariant::fromValue<DynamicsSample>({ p.time,
                                                             p.input,
                                                             p.output,
                                                             p.follower }));
    }
    emit newSamples(samples);
}

void TimelineSourceModel::setIsClipping(bool clipping)
{
    if (m_isClipping == clipping) {
        return;
    }
    m_isClipping = clipping;
    emit isClippingChanged();
}

void TimelineSourceModel::initializeForPlayback(double sampleRate)
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }

    mHistory.emplace(sampleRate);

    // We don't know for sure the least packet size (which is variable). 100
    // samples per packet at a rate of 8kHz is 12.5ms, which is quite low
    // latency. For higher sample rates that will be less.
    constexpr auto leastPacketSize = 100;
    const size_t maxQueueSize = DynamicRangeProcessorHistory::maxTimeSeconds * sampleRate / leastPacketSize;
    mPacketBuffer.reserve(maxQueueSize);

    // Although `mOutputQueue` is a shared_ptr, we construct a unique_ptr and
    // invoke the shared_ptr ctor overload that takes a unique_ptr.
    // This way, we avoid the `error: aligned deallocation function of type
    // 'void (void *, std::align_val_t) noexcept' is only available on
    // macOS 10.13 or newer` compilation error.
    mOutputQueue = std::make_unique<DynamicRangeProcessorOutputPacketQueue>(maxQueueSize);

    instance->SetOutputQueue(mOutputQueue);
    m_deliveryTimer->start();
}
} // namespace au::effects
