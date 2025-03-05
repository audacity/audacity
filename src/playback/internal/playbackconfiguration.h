/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../iplaybackconfiguration.h"

namespace au::playback {
class PlaybackConfiguration : public IPlaybackConfiguration
{
public:
    void init();

    muse::draw::Color playColor() const override;

    std::vector<std::string> playbackQualityList() const override;
    std::string currentPlaybackQuality() const override;
    void setPlaybackQuality(const std::string& quality) override;
    muse::async::Notification playbackQualityChanged() const override;

    std::vector<std::string> ditheringList() const override;
    std::string currentDithering() const override;
    void setDithering(const std::string& dithering) override;
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
