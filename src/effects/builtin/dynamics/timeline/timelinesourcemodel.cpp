/*
 * Audacity: A Digital Audio Editor
 */
#include "timelinesourcemodel.h"

#include "libraries/lib-builtin-effects/CompressorInstance.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorHistory.h"

#include "global/log.h"

#include <QSGGeometryNode>
#include <QTimer>
#include <QtQuick/qsgflatcolormaterial.h>

namespace au::effects {
namespace {
constexpr auto deliveryPeriodMs = 50;
}

void TimelineSourceModel::doInit()
{
    m_deliveryTimer = new QTimer(this);
    connect(m_deliveryTimer, &QTimer::timeout, this, [this] { pullData(); });
    m_deliveryTimer->setInterval(deliveryPeriodMs);

    const std::shared_ptr<CompressorInstance> instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }

    m_initializeProcessingSettingsSubscription
        = static_cast<InitializeProcessingSettingsPublisher&>(*instance).Subscribe([&](const std::optional<InitializeProcessingSettings>&
                                                                                       evt) {
        if (evt) {
            initializeForPlayback(evt->sampleRate);
        } else {
            // Stop the timer-based update but keep the history until playback is resumed.
            m_deliveryTimer->stop();
        }
    });

    m_realtimeResumeSubscription = static_cast<RealtimeResumePublisher&>(*instance).Subscribe([this](auto) {
        emit newDataSequence();
    });

    if (const auto& sampleRate = instance->GetSampleRate(); sampleRate.has_value()) {
        initializeForPlayback(*sampleRate);
    }
}

double TimelineSourceModel::latency() const
{
    return deliveryPeriodMs / 1000.0;
}

double TimelineSourceModel::dataPointRate() const
{
    return m_dataPointRate;
}

void TimelineSourceModel::pullData()
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }

    m_packetBuffer.clear();
    ::DynamicRangeProcessorOutputPacket packet;
    while (m_outputQueue->Get(packet)) {
        m_packetBuffer.push_back(packet);
    }
    m_history->Push(m_packetBuffer);

    const DynamicRangeProcessorHistory::PacketView view = m_history->GetViewOnNewPackets();
    if (view.numPackets() == 0) {
        return;
    }

    QVariantList samples;
    samples.reserve(view.numPackets());
    for (auto i = 0; i < view.numPackets(); ++i) {
        const DynamicRangeProcessorHistory::Packet& p = view.at(i);
        samples.append(QVariant::fromValue<DynamicsSample>({ p.time,
                                                             p.input,
                                                             p.output,
                                                             p.follower }));
    }
    emit newSamples(samples);
}

void TimelineSourceModel::initializeForPlayback(double sampleRate)
{
    const auto instance = m_instance.lock();
    IF_ASSERT_FAILED(instance) {
        return;
    }

    m_history.emplace(sampleRate);

    // We don't know for sure the least packet size (which is variable). 100
    // samples per packet at a rate of 8kHz is 12.5ms, which is quite low
    // latency. For higher sample rates that will be less.
    constexpr auto leastPacketSize = 100;
    const size_t maxQueueSize = DynamicRangeProcessorHistory::maxTimeSeconds * sampleRate / leastPacketSize;
    m_packetBuffer.reserve(maxQueueSize);

    // Although `m_outputQueue` is a shared_ptr, we construct a unique_ptr and
    // invoke the shared_ptr ctor overload that takes a unique_ptr.
    // This way, we avoid the `error: aligned deallocation function of type
    // 'void (void *, std::align_val_t) noexcept' is only available on
    // macOS 10.13 or newer` compilation error.
    m_outputQueue = std::make_unique<DynamicRangeProcessorOutputPacketQueue>(maxQueueSize);

    instance->SetOutputQueue(m_outputQueue);

    assert(instance->GetBlockSize() > 0);
    if (instance->GetBlockSize() > 0) {
        m_dataPointRate = sampleRate / instance->GetBlockSize();
        emit dataPointRateChanged();
    }

    m_deliveryTimer->start();
}
} // namespace au::effects
