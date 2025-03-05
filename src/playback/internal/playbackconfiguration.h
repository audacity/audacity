/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "ui/iuiconfiguration.h"

#include "../iplaybackconfiguration.h"

namespace au::playback {
class PlaybackConfiguration : public IPlaybackConfiguration
{
    muse::Inject<muse::ui::IUiConfiguration> uiConfiguration;

public:
    void init();

    muse::draw::Color playColor() const override;

    TimecodeFormatType playbackTimeItemFormat() const override;
    void setPlaybackTimeItemFormat(TimecodeFormatType format) override;
    muse::async::Notification playbackTimeItemFormatChanged() const override;

    std::vector<PlaybackQualityPrefs::PlaybackQuality> playbackQualityList() const override;
    PlaybackQualityPrefs::PlaybackQuality currentPlaybackQuality() const override;
    void setPlaybackQuality(PlaybackQualityPrefs::PlaybackQuality quality) override;
    muse::async::Notification playbackQualityChanged() const override;

    std::vector<DitherTypePrefs::DitherType> ditheringList() const override;
    DitherTypePrefs::DitherType currentDithering() const override;
    void setDithering(DitherTypePrefs::DitherType dithering) override;
    muse::async::Notification ditheringChanged() const override;

    TracksBehaviors::SoloBehavior currentSoloBehavior() const override;
    void setSoloBehavior(TracksBehaviors::SoloBehavior behavior) override;
    muse::async::Notification soloBehaviorChanged() const override;

    trackedit::secs_t shortSkip() const override;
    void setShortSkip(trackedit::secs_t seconds) override;
    muse::async::Notification shortSkipChanged() const override;

    trackedit::secs_t longSkip() const override;
    void setLongSkip(trackedit::secs_t seconds) override;
    muse::async::Notification longSkipChanged() const override;

private:
    muse::async::Notification m_playbackQualityChanged;
    muse::async::Notification m_ditheringChanged;
    muse::async::Notification m_soloBehaviorChanged;
    muse::async::Notification m_shortSkipChanged;
    muse::async::Notification m_longSkipChanged;
};
}
