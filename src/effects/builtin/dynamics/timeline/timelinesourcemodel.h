/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "dynamicstimelinetypes.h"
#include "abstractdynamicseffectinstancemodel.h"

#include "libraries/lib-utility/Observer.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorHistory.h"

#include <optional>

namespace au::effects {
class TimelineSourceModel : public AbstractDynamicsEffectInstanceModel
{
    Q_OBJECT

    Q_PROPERTY(double dataPointRate READ dataPointRate NOTIFY dataPointRateChanged)
    Q_PROPERTY(double audioThreadBufferDuration READ audioThreadBufferDuration NOTIFY dataPointRateChanged)

public:
    double dataPointRate() const;
    double audioThreadBufferDuration() const;
    Q_INVOKABLE void pullData();

signals:
    void newSamples(const QVariantList& samples);
    void dataPointRateChanged();
    void newDataSequence();

private:
    void doInit() override;
    void initializeForPlayback(double sampleRate, int audioThreadBufferSize);

    std::shared_ptr<::DynamicRangeProcessorOutputPacketQueue> m_outputQueue;
    std::optional<::DynamicRangeProcessorHistory> m_history;
    std::vector<::DynamicRangeProcessorOutputPacket> m_packetBuffer;
    ::Observer::Subscription m_initializeProcessingSettingsSubscription;
    ::Observer::Subscription m_realtimeResumeSubscription;

    // Some plausible non-zero default, to make the initialization of the view simpler
    // even if playback hasn't started (and hence we don't know block size or sample rate) yet.
    double m_dataPointRate = 44100.0 / 512;
    double m_audioThreadBufferDuration = 0;

    QTimer* m_deliveryTimer = nullptr;
};
} // namespace au::effects
