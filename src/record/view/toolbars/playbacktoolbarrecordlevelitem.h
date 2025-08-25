/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "modularity/ioc.h"
#include "record/irecord.h"
#include "record/irecordconfiguration.h"
#include "record/irecordcontroller.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/iaudiodevicesprovider.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::record {
class PlaybackToolBarRecordLevelItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(float level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftRecentPeak READ leftRecentPeak NOTIFY leftRecentPeakChanged FINAL)
    Q_PROPERTY(float leftMaxPeak READ leftMaxPeak NOTIFY leftMaxPeakChanged FINAL)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightRecentPeak READ rightRecentPeak NOTIFY rightRecentPeakChanged FINAL)
    Q_PROPERTY(float rightMaxPeak READ rightMaxPeak NOTIFY rightMaxPeakChanged FINAL)

    Q_PROPERTY(int recordingChannelsCount READ recordingChannelsCount NOTIFY recordingChannelsCountChanged FINAL)

    Q_PROPERTY(
        bool audibleInputMonitoring READ audibleInputMonitoring WRITE setAudibleInputMonitoring NOTIFY audibleInputMonitoringChanged FINAL)
    Q_PROPERTY(bool isMicMeteringOn READ isMicMeteringOn WRITE setIsMicMeteringOn NOTIFY isMicMeteringOnChanged FINAL)

    Q_PROPERTY(playback::PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle NOTIFY meterStyleChanged FINAL)

    muse::Inject<record::IRecord> record;
    muse::Inject<record::IRecordConfiguration> recordConfiguration;
    muse::Inject<record::IRecordController> recordController;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

public:
    explicit PlaybackToolBarRecordLevelItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                            QObject* parent = nullptr);

    float level() const;
    void setLevel(float newLevel);

    float leftChannelPressure() const;
    float leftRecentPeak() const;
    float leftMaxPeak() const;

    float rightChannelPressure() const;
    float rightRecentPeak() const;
    float rightMaxPeak() const;

    int recordingChannelsCount() const;

    bool audibleInputMonitoring() const;
    bool isMicMeteringOn() const;

    playback::PlaybackMeterStyle::MeterStyle meterStyle() const;

    Q_INVOKABLE void listenMainAudioInput(bool listen);

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setLeftRecentPeak(float newLeftRecentPeak);
    void setLeftMaxPeak(float newLeftMaxPeak);

    void setRightChannelPressure(float rightChannelPressure);
    void setRightRecentPeak(float newRightRecentPeak);
    void setRightMaxPeak(float newRightMaxPeak);

    void setAudibleInputMonitoring(bool enable);
    void setIsMicMeteringOn(bool enable);

signals:
    void levelChanged();

    void leftChannelPressureChanged(float leftChannelPressure);
    void leftRecentPeakChanged();
    void leftMaxPeakChanged();

    void rightChannelPressureChanged(float rightChannelPressure);
    void rightRecentPeakChanged();
    void rightMaxPeakChanged();

    void recordingChannelsCountChanged();

    void audibleInputMonitoringChanged();
    void isMicMeteringOnChanged();

    void meterStyleChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    int m_level = 0;

    float m_leftChannelPressure = 0.0;
    float m_leftRecentPeak = 0.0;
    float m_leftMaxPeak = 0.0;

    float m_rightChannelPressure = 0.0;
    float m_rightRecentPeak = 0.0;
    float m_rightMaxPeak = 0.0;
};
}
