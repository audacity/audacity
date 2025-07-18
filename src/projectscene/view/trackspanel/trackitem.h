/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_TRACKITEM_H
#define AU_PROJECTSCENE_TRACKITEM_H

#include <QObject>

#include "modularity/ioc.h"
#include "playback/itrackplaybackcontrol.h"
#include "trackedit/itrackeditinteraction.h"
#include "playback/iplayback.h"
#include "record/irecord.h"

#include "async/asyncable.h"

#include "trackedit/trackedittypes.h"
#include "trackedit/dom/track.h"

namespace au::projectscene {
class TrackItem : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(bool outputOnly READ outputOnly CONSTANT)

    Q_PROPERTY(QVariant trackId READ trackId_property CONSTANT)
    Q_PROPERTY(QString title READ title WRITE setTitle NOTIFY titleChanged)
    Q_PROPERTY(int channelCount READ channelCount NOTIFY channelCountChanged)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)
    Q_PROPERTY(float leftChannelRMS READ leftChannelRMS NOTIFY leftChannelRMSChanged)
    Q_PROPERTY(float rightChannelRMS READ rightChannelRMS NOTIFY rightChannelRMSChanged)

    Q_PROPERTY(float volumeLevel READ volumeLevel NOTIFY volumeLevelChanged)
    Q_PROPERTY(int pan READ pan NOTIFY panChanged)
    Q_PROPERTY(bool solo READ solo WRITE setSolo NOTIFY soloChanged)
    Q_PROPERTY(bool muted READ muted WRITE setMuted NOTIFY mutedChanged)

    Q_PROPERTY(bool isSelected READ isSelected NOTIFY isSelectedChanged)
    Q_PROPERTY(bool isFocused READ isFocused NOTIFY isFocusedChanged)

    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;
    muse::Inject<playback::IPlayback> playback;
    muse::Inject<record::IRecord> record;

public:
    explicit TrackItem(QObject* parent = nullptr);

    ~TrackItem() override;

    void init(const trackedit::Track& track);

    trackedit::TrackId trackId() const;
    QVariant trackId_property() const;
    QString title() const;
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

    // void loadSoloMuteState(const project::IProjectSoloMuteState::SoloMuteState& newState);

    // void subscribeOnAudioSignalChanges(muse::audio::AudioSignalChanges&& audioSignalChanges);

    bool outputOnly() const;

    const audio::AudioOutputParams& outputParams() const;

    bool isSelected() const;
    void setIsSelected(bool selected);

    bool isFocused() const;
    void setIsFocused(bool focused);

public slots:
    void setTitle(QString title);

    void setLeftChannelPressure(float leftChannelPressure);
    void setRightChannelPressure(float rightChannelPressure);
    void setLeftChannelRMS(float leftChannelRMS);
    void setRightChannelRMS(float rightChannelRMS);

    void setSolo(bool solo);
    void setMuted(bool mute);

signals:
    void titleChanged(QString title);

    void leftChannelPressureChanged(float leftChannelPressure);
    void rightChannelPressureChanged(float rightChannelPressure);
    void leftChannelRMSChanged(float leftChannelRMS);
    void rightChannelRMSChanged(float rightChannelRMS);

    void volumeLevelChanged(float volumeLevel);
    void panChanged(int pan);
    void soloChanged();
    void mutedChanged();
    void channelCountChanged();

    void outputParamsChanged(const audio::AudioOutputParams& params);

    // void soloMuteStateChanged(const project::IProjectSoloMuteState::SoloMuteState& state);

    void inputResourceItemChanged();
    void outputResourceItemListChanged();
    void auxSendItemListChanged();

    void isSelectedChanged();
    void isFocusedChanged();

protected:
    void setAudioChannelVolumePressure(const trackedit::audioch_t chNum, const float newValue);
    void setAudioChannelRMS(const trackedit::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> m_playbackTrackSignalChanged;
    muse::async::Channel<au::audio::audioch_t, au::audio::MeterSignal> m_recordTrackSignalChanged;
    audio::AudioOutputParams m_outParams;

    trackedit::TrackId m_trackId = -1;
    trackedit::TrackType m_trackType = trackedit::TrackType::Undefined;
    QString m_title;
    bool m_outputOnly = false;

    float m_leftChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelPressure = playback::MIN_DISPLAYED_DBFS;
    float m_leftChannelRMS = playback::MIN_DISPLAYED_DBFS;
    float m_rightChannelRMS = playback::MIN_DISPLAYED_DBFS;

    bool m_outputResourceItemsLoading = false;

    bool m_isSelected = false;
    bool m_isFocused = false;
};
}

#endif // AU_PROJECTSCENE_TRACKITEM_H
