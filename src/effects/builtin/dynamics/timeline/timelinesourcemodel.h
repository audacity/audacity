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

    Q_PROPERTY(bool isClipping READ isClipping WRITE setIsClipping NOTIFY isClippingChanged)
    Q_PROPERTY(double dataPointRate READ dataPointRate NOTIFY dataPointRateChanged)
    Q_PROPERTY(double latency READ latency CONSTANT)

public:
    bool isClipping() const { return m_isClipping; }
    void setIsClipping(bool clipping);
    double dataPointRate() const;
    double latency() const;

signals:
    void isClippingChanged();
    void newSamples(const QVariantList& samples);
    void tChanged();
    void dataPointRateChanged();

private:
    void doInit() override;
    void pullData();
    void initializeForPlayback(double sampleRate);

    std::shared_ptr<::DynamicRangeProcessorOutputPacketQueue> m_outputQueue;
    std::optional<::DynamicRangeProcessorHistory> m_history;
    std::vector<::DynamicRangeProcessorOutputPacket> m_packetBuffer;
    ::Observer::Subscription m_initializeProcessingSettingsSubscription;

    bool m_isClipping = false;
    QTimer* m_deliveryTimer = nullptr;
};
} // namespace au::effects
