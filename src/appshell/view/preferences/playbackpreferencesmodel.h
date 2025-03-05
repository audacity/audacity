/*
 * Audacity: A Digital Audio Editor
 */
#ifndef AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H
#define AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H

#include <QObject>

#include "modularity/ioc.h"

#include "async/asyncable.h"
#include "audio/iaudioconfiguration.h"

#include "playback/iaudiodevicesprovider.h"
#include "playback/iplaybackconfiguration.h"

namespace au::appshell {
class PlaybackPreferencesModel : public QObject, public muse::async::Asyncable
{
    Q_OBJECT

    Q_PROPERTY(
        playback::PlaybackQualityPrefs::PlaybackQuality currentPlaybackQuality READ currentPlaybackQuality NOTIFY currentPlaybackQualityChanged)
    Q_PROPERTY(QVariantList playbackQualityList READ playbackQualityList CONSTANT)

    Q_PROPERTY(playback::DitherTypePrefs::DitherType currentDithering READ currentDithering NOTIFY currentDitheringChanged)
    Q_PROPERTY(QVariantList ditheringList READ ditheringList CONSTANT)

    Q_PROPERTY(playback::TracksBehaviors::SoloBehavior soloBehavior READ soloBehavior NOTIFY soloBehaviorChanged)

    Q_PROPERTY(double shortSkip READ shortSkip NOTIFY shortSkipChanged)
    Q_PROPERTY(double longSkip READ longSkip NOTIFY longSkipChanged)

    muse::Inject<muse::audio::IAudioConfiguration> audioConfiguration;
    muse::Inject<playback::IPlaybackConfiguration> playbackConfiguration;
    muse::Inject<playback::IAudioDevicesProvider> audioDevicesProvider;

public:
    explicit PlaybackPreferencesModel(QObject* parent = nullptr);

    Q_INVOKABLE void init();

    playback::PlaybackQualityPrefs::PlaybackQuality currentPlaybackQuality() const;
    QVariantList playbackQualityList() const;
    Q_INVOKABLE void setPlaybackQuality(playback::PlaybackQualityPrefs::PlaybackQuality quality);

    playback::DitherTypePrefs::DitherType currentDithering() const;
    QVariantList ditheringList() const;
    Q_INVOKABLE void setDithering(playback::DitherTypePrefs::DitherType quality);

    Q_INVOKABLE playback::TracksBehaviors::SoloBehavior soloBehavior() const;
    Q_INVOKABLE void setSoloBehavior(playback::TracksBehaviors::SoloBehavior behavior);

    double shortSkip() const;
    Q_INVOKABLE void setShortSkip(double seconds);

    double longSkip() const;
    Q_INVOKABLE void setLongSkip(double seconds);

signals:
    void currentPlaybackQualityChanged();
    void currentDitheringChanged();

    void soloBehaviorChanged();

    void shortSkipChanged();
    void longSkipChanged();
};
}

#endif // AU_APPSHELL_PLAYBACKPREFERENCESMODEL_H
