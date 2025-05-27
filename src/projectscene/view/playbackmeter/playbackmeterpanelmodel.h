/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/playbacktypes.h"
#include "playback/iaudiooutput.h"

namespace au::projectscene {
class PlaybackMeterPanelModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<au::playback::IPlayback> playback;
    muse::Inject<au::playback::IPlaybackConfiguration> configuration;

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)

    Q_PROPERTY(playback::PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle NOTIFY meterStyleChanged FINAL)
    Q_PROPERTY(playback::PlaybackMeterType::MeterType meterType READ meterType NOTIFY meterTypeChanged FINAL)
    Q_PROPERTY(playback::PlaybackMeterPosition::MeterPosition meterPosition READ meterPosition NOTIFY meterPositionChanged FINAL)

    Q_PROPERTY(float level READ level NOTIFY levelChanged FINAL)

public:
    explicit PlaybackMeterPanelModel(QObject* parent = nullptr);

    float leftChannelPressure() const;
    float leftChannelRMS() const;

    float rightChannelPressure() const;
    float rightChannelRMS() const;

    playback::PlaybackMeterStyle::MeterStyle meterStyle() const;
    playback::PlaybackMeterType::MeterType meterType() const;
    playback::PlaybackMeterPosition::MeterPosition meterPosition() const;

    float level() const;

    Q_INVOKABLE void positionChangeRequested(playback::PlaybackMeterPosition::MeterPosition position);
    Q_INVOKABLE void styleChangeRequested(playback::PlaybackMeterStyle::MeterStyle style);
    Q_INVOKABLE void typeChangeRequested(playback::PlaybackMeterType::MeterType type);
    Q_INVOKABLE void volumeLevelChangeRequested(float level);

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setLeftChannelRMS(float leftChannelRMS);

    void setRightChannelPressure(float rightChannelPressure);
    void setRightChannelRMS(float rightChannelRMS);

signals:
    void leftChannelPressureChanged(float leftChannelPressure);
    void leftChannelRMSChanged(float leftChannelRMS);

    void rightChannelPressureChanged(float rightChannelPressure);
    void rightChannelRMSChanged(float rightChannelRMS);

    void meterStyleChanged();
    void meterTypeChanged();
    void meterPositionChanged();

    void levelChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    float m_leftChannelPressure = -60.0;
    float m_leftChannelRMS = -60.0;

    float m_rightChannelPressure = -60.0;
    float m_rightChannelRMS = -60.0;

    float m_level = 0;
};
}
