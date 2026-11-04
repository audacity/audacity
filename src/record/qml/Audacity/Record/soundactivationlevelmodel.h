/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QtQml/qqmlregistration.h>

#include "framework/global/modularity/ioc.h"

#include "audio/driver/iaudiodrivercontroller.h"
#include "playback/view/common/metermodel.h"
#include "record/irecord.h"
#include "record/irecordconfiguration.h"
#include "record/irecordmetercontroller.h"

namespace au::record {
class SoundActivationLevelModel : public playback::MeterModel
{
    Q_OBJECT
    QML_ELEMENT

    Q_PROPERTY(double level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(bool listening READ listening WRITE setListening NOTIFY listeningChanged FINAL)
    Q_PROPERTY(int recordingChannelsCount READ recordingChannelsCount NOTIFY recordingChannelsCountChanged FINAL)
    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY channelsSignalChanged FINAL)
    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY channelsSignalChanged FINAL)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY channelsSignalChanged FINAL)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY channelsSignalChanged FINAL)

    muse::GlobalInject<IRecordConfiguration> recordConfiguration;
    muse::GlobalInject<IRecordMeterController> recordMeterController;
    muse::GlobalInject<audio::IAudioDriverController> audioDriverController;
    muse::ContextInject<IRecord> record{ this };

public:
    explicit SoundActivationLevelModel(QObject* parent = nullptr);
    ~SoundActivationLevelModel() override;

    Q_INVOKABLE void init() override;
    Q_INVOKABLE void apply();

    double level() const;
    void setLevel(double level);

    bool listening() const;
    void setListening(bool listening);

    int recordingChannelsCount() const;

    float leftChannelPressure() const;
    float rightChannelPressure() const;
    float leftChannelRMS() const;
    float rightChannelRMS() const;

signals:
    void levelChanged();
    void listeningChanged();
    void recordingChannelsCountChanged();
    void channelsSignalChanged();

private:
    void setChannelSignal(audio::audioch_t channel, const audio::MeterSignal& signal);
    void resetChannelsSignal();

    double m_level = 0.0;
    bool m_listening = false;

    float m_leftChannelPressure = 0.0f;
    float m_rightChannelPressure = 0.0f;
    float m_leftChannelRMS = 0.0f;
    float m_rightChannelRMS = 0.0f;
};
}
