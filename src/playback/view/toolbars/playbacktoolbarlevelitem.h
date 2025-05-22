/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <QString>

#include "modularity/ioc.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"

#include "uicomponents/view/toolbaritem.h"

namespace au::playback {
class PlaybackToolBarLevelItem : public muse::uicomponents::ToolBarItem
{
    Q_OBJECT

    Q_PROPERTY(int level READ level WRITE setLevel NOTIFY levelChanged FINAL)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)
    Q_PROPERTY(float leftRecentPeak READ leftRecentPeak NOTIFY leftRecentPeakChanged FINAL)
    Q_PROPERTY(float leftMaxPeak READ leftMaxPeak NOTIFY leftMaxPeakChanged FINAL)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)
    Q_PROPERTY(float rightRecentPeak READ rightRecentPeak NOTIFY rightRecentPeakChanged FINAL)
    Q_PROPERTY(float rightMaxPeak READ rightMaxPeak NOTIFY rightMaxPeakChanged FINAL)

    Q_PROPERTY(PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle WRITE setMeterStyle NOTIFY meterStyleChanged FINAL)
    Q_PROPERTY(PlaybackMeterType::MeterType meterType READ meterType WRITE setMeterType NOTIFY meterTypeChanged FINAL)
    Q_PROPERTY(
        PlaybackMeterPosition::MeterPosition meterPosition READ meterPosition WRITE setMeterPosition NOTIFY meterPositionChanged FINAL)

    muse::Inject<IPlayback> playback;
    muse::Inject<IPlaybackConfiguration> configuration;

public:
    explicit PlaybackToolBarLevelItem(const muse::ui::UiAction& action, muse::uicomponents::ToolBarItemType::Type type,
                                      QObject* parent = nullptr);

    int level() const;
    void setLevel(int newLevel);

    float leftChannelPressure() const;
    float leftChannelRMS() const;
    float leftRecentPeak() const;
    float leftMaxPeak() const;

    float rightChannelPressure() const;
    float rightChannelRMS() const;
    float rightRecentPeak() const;
    float rightMaxPeak() const;

    PlaybackMeterStyle::MeterStyle meterStyle() const;
    PlaybackMeterType::MeterType meterType() const;
    PlaybackMeterPosition::MeterPosition meterPosition() const;

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setLeftChannelRMS(float leftChannelRMS);
    void setLeftRecentPeak(float newLeftRecentPeak);
    void setLeftMaxPeak(float newLeftMaxPeak);

    void setRightChannelPressure(float rightChannelPressure);
    void setRightChannelRMS(float rightChannelRMS);
    void setRightRecentPeak(float newRightRecentPeak);
    void setRightMaxPeak(float newRightMaxPeak);

    void setMeterStyle(PlaybackMeterStyle::MeterStyle style);
    void setMeterType(PlaybackMeterType::MeterType type);
    void setMeterPosition(PlaybackMeterPosition::MeterPosition position);

signals:
    void levelChanged();

    void leftChannelPressureChanged(float leftChannelPressure);
    void leftChannelRMSChanged(float leftChannelRMS);
    void leftRecentPeakChanged();
    void leftMaxPeakChanged();

    void rightChannelPressureChanged(float rightChannelPressure);
    void rightChannelRMSChanged(float rightChannelRMS);
    void rightRecentPeakChanged();
    void rightMaxPeakChanged();

    void meterStyleChanged();
    void meterTypeChanged();
    void meterPositionChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    int m_level = 0;

    float m_leftChannelPressure = -60.0;
    float m_leftChannelRMS = -60.0;
    float m_leftRecentPeak = -60.0;
    float m_leftMaxPeak = -60.0;

    float m_rightChannelPressure = -60.0;
    float m_rightChannelRMS = -60.0;
    float m_rightRecentPeak = -60.0;
    float m_rightMaxPeak = -60.0;

    PlaybackMeterStyle::MeterStyle m_meterStyle = PlaybackMeterStyle::MeterStyle::Default;
    PlaybackMeterType::MeterType m_meterType = PlaybackMeterType::MeterType::DbLog;
    PlaybackMeterPosition::MeterPosition m_meterPosition = PlaybackMeterPosition::MeterPosition::TopBar;
};
}
