/*
* Audacity: A Digital Audio Editor
*/
#ifndef AU_PROJECTSCENE_TRACKITEM_H
#define AU_PROJECTSCENE_TRACKITEM_H

#include <QObject>

#include "modularity/ioc.h"
#include "playback/itrackplaybackcontrol.h"
#include "trackedit/itrackeditinteraction.h"

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
    Q_PROPERTY(int channelCount READ channelCount CONSTANT)

    Q_PROPERTY(float leftChannelPressure READ leftChannelPressure NOTIFY leftChannelPressureChanged)
    Q_PROPERTY(float rightChannelPressure READ rightChannelPressure NOTIFY rightChannelPressureChanged)

    Q_PROPERTY(float volumeLevel READ volumeLevel WRITE setVolumeLevel NOTIFY volumeLevelChanged)
    Q_PROPERTY(int balance READ balance WRITE setBalance NOTIFY balanceChanged)
    Q_PROPERTY(bool solo READ solo WRITE setSolo NOTIFY soloChanged)
    Q_PROPERTY(bool muted READ muted WRITE setMuted NOTIFY mutedChanged)
    Q_PROPERTY(bool forceMute READ forceMute NOTIFY forceMuteChanged)

    Q_PROPERTY(bool isSelected READ isSelected NOTIFY isSelectedChanged)

    muse::Inject<playback::ITrackPlaybackControl> trackPlaybackControl;
    muse::Inject<trackedit::ITrackeditInteraction> trackeditInteraction;

public:
    TrackItem(QObject* parent = nullptr);

    ~TrackItem() override;

    void init(const trackedit::Track& track);

    trackedit::TrackId trackId() const;
    QVariant trackId_property() const;
    QString title() const;
    int channelCount() const;

    float leftChannelPressure() const;
    float rightChannelPressure() const;

    float volumeLevel() const;
    int balance() const;
    bool solo() const;
    bool muted() const;
    bool forceMute() const;

    void loadOutputParams(const audio::AudioOutputParams& newParams);

    // void loadSoloMuteState(const project::IProjectSoloMuteState::SoloMuteState& newState);

    // void subscribeOnAudioSignalChanges(muse::audio::AudioSignalChanges&& audioSignalChanges);

    bool outputOnly() const;

    const audio::AudioOutputParams& outputParams() const;

    bool isSelected() const;
    void setIsSelected(bool selected);

public slots:
    void setTitle(QString title);

    void setLeftChannelPressure(float leftChannelPressure);
    void setRightChannelPressure(float rightChannelPressure);

    void setVolumeLevel(float volumeLevel);
    void setBalance(int balance);
    void setSolo(bool solo);
    void setMuted(bool mute);

signals:
    void titleChanged(QString title);

    void leftChannelPressureChanged(float leftChannelPressure);
    void rightChannelPressureChanged(float rightChannelPressure);

    void volumeLevelChanged(float volumeLevel);
    void balanceChanged(int balance);
    void soloChanged();
    void mutedChanged();
    void forceMuteChanged();

    void outputParamsChanged(const audio::AudioOutputParams& params);

    // void soloMuteStateChanged(const project::IProjectSoloMuteState::SoloMuteState& state);

    void inputResourceItemChanged();
    void outputResourceItemListChanged();
    void auxSendItemListChanged();

    void isSelectedChanged();

protected:
    void setAudioChannelVolumePressure(const trackedit::audioch_t chNum, const float newValue);
    void resetAudioChannelsVolumePressure();

    void applyMuteToOutputParams(const bool isMuted);

    void addBlankSlots(size_t count);
    void removeBlankSlotsFromEnd(size_t count);

    // muse::audio::AudioSignalChanges m_audioSignalChanges;
    audio::AudioOutputParams m_outParams;

    trackedit::TrackId m_trackId = -1;
    trackedit::TrackType m_trackType = trackedit::TrackType::Undefined;
    QString m_title;
    bool m_outputOnly = false;

    float m_leftChannelPressure = 0.0;
    float m_rightChannelPressure = 0.0;

    bool m_outputResourceItemsLoading = false;

    bool m_isSelected = false;
};
}

#endif // AU_PROJECTSCENE_TRACKITEM_H
