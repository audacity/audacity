/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/asyncable.h"
#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "playback/view/common/playbackmetermodel.h"
#include "playback/iplayback.h"
#include "playback/iplaybackconfiguration.h"
#include "playback/playbacktypes.h"
#include "playback/iaudiooutput.h"
#include "playback/iplaybackcontroller.h"

namespace au::playback {
class PlaybackMeterPanelModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    muse::Inject<IPlayback> playback;
    muse::Inject<IPlaybackConfiguration> configuration;
    muse::Inject<IPlaybackController> controller;

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)

    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)

    Q_PROPERTY(PlaybackMeterStyle::MeterStyle meterStyle READ meterStyle NOTIFY meterStyleChanged FINAL)
    Q_PROPERTY(PlaybackMeterType::MeterType meterType READ meterType NOTIFY meterTypeChanged FINAL)
    Q_PROPERTY(PlaybackMeterPosition::MeterPosition meterPosition READ meterPosition NOTIFY meterPositionChanged FINAL)

    Q_PROPERTY(PlaybackMeterModel * meterModel READ meterModel NOTIFY meterModelChanged FINAL)

    Q_PROPERTY(float level READ level NOTIFY levelChanged FINAL)

    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)

public:
    explicit PlaybackMeterPanelModel(QObject* parent = nullptr);

    float leftChannelPressure() const;
    float leftChannelRMS() const;

    float rightChannelPressure() const;
    float rightChannelRMS() const;

    PlaybackMeterStyle::MeterStyle meterStyle() const;
    PlaybackMeterType::MeterType meterType() const;
    PlaybackMeterPosition::MeterPosition meterPosition() const;
    PlaybackMeterModel* meterModel() const;

    float level() const;

    bool isPlaying() const;

    Q_INVOKABLE void positionChangeRequested(PlaybackMeterPosition::MeterPosition position);
    Q_INVOKABLE void styleChangeRequested(PlaybackMeterStyle::MeterStyle style);
    Q_INVOKABLE void typeChangeRequested(PlaybackMeterType::MeterType type);
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
    void meterModelChanged();

    void levelChanged();

    void isPlayingChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    PlaybackMeterModel* m_meterModel = nullptr;

    float m_leftChannelPressure = -60.0;
    float m_leftChannelRMS = -60.0;

    float m_rightChannelPressure = -60.0;
    float m_rightChannelRMS = -60.0;

    float m_level = 0;
};
}
