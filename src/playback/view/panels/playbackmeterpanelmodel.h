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

    Q_PROPERTY(PlaybackMeterModel * meterModel READ meterModel NOTIFY meterModelChanged FINAL)

    Q_PROPERTY(float level READ level NOTIFY levelChanged FINAL)

    Q_PROPERTY(bool isPlaying READ isPlaying NOTIFY isPlayingChanged FINAL)

public:
    explicit PlaybackMeterPanelModel(QObject* parent = nullptr);

    float leftChannelPressure() const;
    float leftChannelRMS() const;

    float rightChannelPressure() const;
    float rightChannelRMS() const;

    PlaybackMeterModel* meterModel() const;

    float level() const;

    bool isPlaying() const;

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

    void meterModelChanged();

    void levelChanged();

    void isPlayingChanged();

private:
    void setAudioChannelVolumePressure(const audio::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const audio::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    PlaybackMeterModel* m_meterModel = nullptr;

    float m_leftChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_leftChannelRMS = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelRMS = playback::MIN_DISPLAYED_DBFS;

    float m_level = 0;
};
}
