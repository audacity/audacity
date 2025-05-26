/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/playbacktypes.h"
#include "playback/iaudiooutput.h"
#include "ui/iuiconfiguration.h"

namespace au::projectscene {
class PlaybackMeterModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::playback::IPlayback> playback;
    muse::Inject<au::playback::IPlaybackConfiguration> configuration;
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)

    Q_PROPERTY(playback::PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle WRITE setMeterStyle NOTIFY meterStyleChanged FINAL)
    Q_PROPERTY(playback::PlaybackMeterType::MeterType meterType READ meterType WRITE setMeterType NOTIFY meterTypeChanged FINAL)
    Q_PROPERTY(
        playback::PlaybackMeterPosition::MeterPosition meterPosition READ meterPosition WRITE setMeterPosition NOTIFY meterPositionChanged FINAL)
    Q_PROPERTY(bool visible READ visible NOTIFY visibleChanged FINAL)

public:
    explicit PlaybackMeterModel(QObject* parent = nullptr);

    float leftChannelPressure() const;
    float leftChannelRMS() const;

    float rightChannelPressure() const;
    float rightChannelRMS() const;

    playback::PlaybackMeterStyle::MeterStyle meterStyle() const;
    playback::PlaybackMeterType::MeterType meterType() const;
    playback::PlaybackMeterPosition::MeterPosition meterPosition() const;

    bool visible() const;

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setLeftChannelRMS(float leftChannelRMS);

    void setRightChannelPressure(float rightChannelPressure);
    void setRightChannelRMS(float rightChannelRMS);

    void setMeterStyle(playback::PlaybackMeterStyle::MeterStyle style);
    void setMeterType(playback::PlaybackMeterType::MeterType type);
    void setMeterPosition(playback::PlaybackMeterPosition::MeterPosition position);

signals:
    void leftChannelPressureChanged(float leftChannelPressure);
    void leftChannelRMSChanged(float leftChannelRMS);

    void rightChannelPressureChanged(float rightChannelPressure);
    void rightChannelRMSChanged(float rightChannelRMS);

    void meterStyleChanged();
    void meterTypeChanged();
    void meterPositionChanged();

    void visibleChanged();
private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    float m_leftChannelPressure = -60.0;
    float m_leftChannelRMS = -60.0;

    float m_rightChannelPressure = -60.0;
    float m_rightChannelRMS = -60.0;

    playback::PlaybackMeterStyle::MeterStyle m_meterStyle = playback::PlaybackMeterStyle::MeterStyle::Default;
    playback::PlaybackMeterType::MeterType m_meterType = playback::PlaybackMeterType::MeterType::DbLog;
    playback::PlaybackMeterPosition::MeterPosition m_meterPosition = playback::PlaybackMeterPosition::MeterPosition::TopBar;

    bool m_visible = true;
};
}
