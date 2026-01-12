/*
 * Audacity: A Digital Audio Editor
 */

#include "log.h"

#include "playbackpreferencesmodel.h"

using namespace au::appshell;
using namespace muse::audio;

PlaybackPreferencesModel::PlaybackPreferencesModel(QObject* parent)
    : QObject(parent)
{
}

void PlaybackPreferencesModel::init()
{
    playbackConfiguration()->playbackQualityChanged().onNotify(this, [this](){ emit currentPlaybackQualityChanged(); });
    playbackConfiguration()->ditheringChanged().onNotify(this, [this](){ emit currentDitheringChanged(); });
    playbackConfiguration()->soloBehaviorChanged().onNotify(this, [this](){
        emit soloBehaviorChanged();
    });
    playbackConfiguration()->shortSkipChanged().onNotify(this, [this](){ emit shortSkipChanged(); });
    playbackConfiguration()->longSkipChanged().onNotify(this, [this](){ emit longSkipChanged(); });
}

au::playback::PlaybackQualityPrefs::PlaybackQuality PlaybackPreferencesModel::currentPlaybackQuality() const
{
    return playbackConfiguration()->currentPlaybackQuality();
}

QVariantList PlaybackPreferencesModel::playbackQualityList() const
{
    QVariantList result;
    for (const auto& quality: playbackConfiguration()->playbackQualityList()) {
        result << static_cast<int>(quality);
    }

    return result;
}

void PlaybackPreferencesModel::setPlaybackQuality(playback::PlaybackQualityPrefs::PlaybackQuality newQuality)
{
    if (currentPlaybackQuality() == newQuality) {
        return;
    }

    playbackConfiguration()->setPlaybackQuality(newQuality);
}

au::playback::DitherTypePrefs::DitherType PlaybackPreferencesModel::currentDithering() const
{
    return playbackConfiguration()->currentDithering();
}

QVariantList PlaybackPreferencesModel::ditheringList() const
{
    QVariantList result;
    for (const auto& dithering: playbackConfiguration()->ditheringList()) {
        result << static_cast<int>(dithering);
    }

    return result;
}

void PlaybackPreferencesModel::setDithering(playback::DitherTypePrefs::DitherType dithering)
{
    if (currentDithering() == dithering) {
        return;
    }

    playbackConfiguration()->setDithering(dithering);
}

au::playback::TracksBehaviors::SoloBehavior PlaybackPreferencesModel::soloBehavior() const
{
    return playbackConfiguration()->currentSoloBehavior();
}

void PlaybackPreferencesModel::setSoloBehavior(playback::TracksBehaviors::SoloBehavior behavior)
{
    if (soloBehavior() == behavior) {
        return;
    }

    playbackConfiguration()->setSoloBehavior(behavior);
}

double PlaybackPreferencesModel::shortSkip() const
{
    return playbackConfiguration()->shortSkip();
}

void PlaybackPreferencesModel::setShortSkip(double seconds)
{
    if (shortSkip() == seconds) {
        return;
    }

    playbackConfiguration()->setShortSkip(seconds);
}

double PlaybackPreferencesModel::longSkip() const
{
    return playbackConfiguration()->longSkip();
}

void PlaybackPreferencesModel::setLongSkip(double seconds)
{
    if (longSkip() == seconds) {
        return;
    }

    playbackConfiguration()->setLongSkip(seconds);
}
