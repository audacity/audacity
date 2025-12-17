/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "audio/iaudiodevicesprovider.h"
#include "playback/iplayback.h"
#include "playback/itrackplaybackcontrol.h"
#include "record/irecord.h"
#include "trackedit/iprojecthistory.h"

#include "trackitem.h"

namespace au::projectscene {
class WaveTrackItem : public TrackItem
{
    Q_OBJECT

    Q_PROPERTY(bool outputOnly READ outputOnly CONSTANT)
    Q_PROPERTY(int channelCount READ channelCount NOTIFY channelCountChanged)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)

    Q_PROPERTY(float volumeLevel READ volumeLevel NOTIFY volumeLevelChanged)
    Q_PROPERTY(int pan READ pan NOTIFY panChanged)
    Q_PROPERTY(bool solo READ solo WRITE setSolo NOTIFY soloChanged)
    Q_PROPERTY(bool muted READ muted WRITE setMuted NOTIFY mutedChanged)

    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;
    muse::Inject<playback::IPlayback> playback;
    muse::Inject<record::IRecord> record;
    muse::Inject<audio::IAudioDevicesProvider> audioDevicesProvider;
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    explicit WaveTrackItem(QObject* parent = nullptr);

    void init(const trackedit::Track& track) override;

    bool outputOnly() const;
    int channelCount() const;

    float leftChannelPressure() const;
    float rightChannelPressure() const;
    float leftChannelRMS() const;
    float rightChannelRMS() const;

    float volumeLevel() const;
    Q_INVOKABLE void setVolumeLevel(float volumeLevel, bool completed);

    int pan() const;
    Q_INVOKABLE void setPan(int pan, bool completed);

    bool solo() const;
    bool muted() const;

    void loadOutputParams(const audio::AudioOutputParams& newParams);

    const audio::AudioOutputParams& outputParams() const;

public slots:
    void setLeftChannelPressure(float leftChannelPressure);
    void setRightChannelPressure(float rightChannelPressure);
    void setLeftChannelRMS(float leftChannelRMS);
    void setRightChannelRMS(float rightChannelRMS);

    void setSolo(bool solo);
    void setMuted(bool mute);

signals:
    void channelCountChanged();

    void leftChannelPressureChanged(float leftChannelPressure);
    void rightChannelPressureChanged(float rightChannelPressure);
    void leftChannelRMSChanged(float leftChannelRMS);
    void rightChannelRMSChanged(float rightChannelRMS);

    void volumeLevelChanged(float volumeLevel);
    void panChanged(int pan);
    void soloChanged();
    void mutedChanged();

    void outputParamsChanged(const audio::AudioOutputParams& params);

protected:
    void setAudioChannelVolumePressure(const trackedit::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const trackedit::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();
    void checkMainAudioInput();
    void muteOrSoloChanged();

    bool isAudible() const override;

private:
    audio::AudioOutputParams m_outParams;

    float m_leftChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_leftChannelRMS = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelRMS = playback::MIN_DISPLAYED_DBFS;

    bool m_outputOnly = false;
    bool m_recordStreamChannelsMatch = 0;
};
}
