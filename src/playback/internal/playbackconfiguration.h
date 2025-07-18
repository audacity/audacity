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

    PlaybackMeterStyle::MeterStyle playbackMeterStyle() const override;
    void setPlaybackMeterStyle(PlaybackMeterStyle::MeterStyle style) override;
    muse::async::Notification playbackMeterStyleChanged() const override;

    PlaybackMeterType::MeterType playbackMeterType() const override;
    void setPlaybackMeterType(PlaybackMeterType::MeterType type) override;
    muse::async::Notification playbackMeterTypeChanged() const override;

    PlaybackMeterPosition::MeterPosition playbackMeterPosition() const override;
    void setPlaybackMeterPosition(PlaybackMeterPosition::MeterPosition position) override;
    muse::async::Notification playbackMeterPositionChanged() const override;

    PlaybackMeterDbRange::DbRange playbackMeterDbRange() const override;
    void setPlaybackMeterDbRange(PlaybackMeterDbRange::DbRange range) override;
    muse::async::Notification playbackMeterDbRangeChanged() const override;

    int playbackHorizontalMeterSize() const override;
    void setPlaybackHorizontalMeterSize(int size) override;
    muse::async::Notification playbackHorizontalMeterSizeChanged() const override;

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
