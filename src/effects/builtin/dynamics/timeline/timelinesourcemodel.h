#pragma once

#include "dynamicstimelinetypes.h"
#include "effects/effects_base/ieffectinstancesregister.h"

#include "modularity/ioc.h"

#include "libraries/lib-utility/Observer.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorTypes.h"
#include "libraries/lib-dynamic-range-processor/DynamicRangeProcessorHistory.h"

#include <QObject>
#include <QTimer>

#include <unordered_map>
#include <optional>

class CompressorInstance;

namespace au::effects {
class TimelineSourceModel : public QObject
{
    Q_OBJECT

    Q_PROPERTY(int instanceId READ instanceId WRITE setInstanceId NOTIFY effectIdChanged FINAL)
    Q_PROPERTY(bool isClipping READ isClipping WRITE setIsClipping NOTIFY
               isClippingChanged)
    Q_PROPERTY(double dataPointRate READ dataPointRate NOTIFY dataPointRateChanged)
    Q_PROPERTY(double latency READ latency CONSTANT)

    muse::Inject<IEffectInstancesRegister> instancesRegister;

public:
    explicit TimelineSourceModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    int instanceId() const { return m_instanceId; }
    void setInstanceId(int id);

    bool isClipping() const { return m_isClipping; }
    void setIsClipping(bool clipping);
    double dataPointRate() const;
    double latency() const;

signals:
    void effectIdChanged();
    void isClippingChanged();
    void newSamples(const QVariantList& samples);
    void tChanged();
    void dataPointRateChanged();

private:
    void pullData();
    void initializeForPlayback(double sampleRate);

    std::weak_ptr<::CompressorInstance> m_instance;
    std::shared_ptr<::DynamicRangeProcessorOutputPacketQueue> mOutputQueue;
    std::optional<::DynamicRangeProcessorHistory> mHistory;
    std::vector<::DynamicRangeProcessorOutputPacket> mPacketBuffer;
    ::Observer::Subscription mInitializeProcessingSettingsSubscription;

    bool m_isClipping = false;
    int m_instanceId = -1;
    QTimer* m_deliveryTimer = nullptr;
};
} // namespace au::effects
