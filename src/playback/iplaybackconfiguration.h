/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "async/notification.h"
#include "modularity/imoduleinterface.h"
#include "draw/types/color.h"

#include "playbacktypes.h"
#include "trackedit/trackedittypes.h"

namespace au::playback {
class IPlaybackConfiguration : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(IPlaybackConfiguration)

public:
    virtual ~IPlaybackConfiguration() = default;

    virtual muse::draw::Color playColor() const = 0;

    virtual TimecodeFormatType playbackTimeItemFormat() const = 0;
    virtual void setPlaybackTimeItemFormat(TimecodeFormatType format) = 0;
    virtual muse::async::Notification playbackTimeItemFormatChanged() const = 0;

    virtual PlaybackMeterStyle::MeterStyle playbackMeterStyle() const = 0;
    virtual void setPlaybackMeterStyle(PlaybackMeterStyle::MeterStyle style) = 0;
    virtual muse::async::Notification playbackMeterStyleChanged() const = 0;

    virtual PlaybackMeterType::MeterType playbackMeterType() const = 0;
    virtual void setPlaybackMeterType(PlaybackMeterType::MeterType type) = 0;
    virtual muse::async::Notification playbackMeterTypeChanged() const = 0;

    virtual PlaybackMeterPosition::MeterPosition playbackMeterPosition() const = 0;
    virtual void setPlaybackMeterPosition(PlaybackMeterPosition::MeterPosition position) = 0;
    virtual muse::async::Notification playbackMeterPositionChanged() const = 0;

    virtual PlaybackMeterDbRange::DbRange playbackMeterDbRange() const = 0;
    virtual void setPlaybackMeterDbRange(PlaybackMeterDbRange::DbRange range) = 0;
    virtual muse::async::Notification playbackMeterDbRangeChanged() const = 0;

    virtual int playbackHorizontalMeterSize() const = 0;
    virtual void setPlaybackHorizontalMeterSize(int size) = 0;
    virtual muse::async::Notification playbackHorizontalMeterSizeChanged() const = 0;

    virtual std::vector<PlaybackQualityPrefs::PlaybackQuality> playbackQualityList() const = 0;
    virtual PlaybackQualityPrefs::PlaybackQuality currentPlaybackQuality() const = 0;
    virtual void setPlaybackQuality(PlaybackQualityPrefs::PlaybackQuality quality) = 0;
    virtual muse::async::Notification playbackQualityChanged() const = 0;

    virtual std::vector<DitherTypePrefs::DitherType> ditheringList() const = 0;
    virtual DitherTypePrefs::DitherType currentDithering() const = 0;
    virtual void setDithering(DitherTypePrefs::DitherType dithering) = 0;
    virtual muse::async::Notification ditheringChanged() const = 0;

    virtual TracksBehaviors::SoloBehavior currentSoloBehavior() const = 0;
    virtual void setSoloBehavior(TracksBehaviors::SoloBehavior behavior) = 0;
    virtual muse::async::Notification soloBehaviorChanged() const = 0;

    virtual trackedit::secs_t shortSkip() const = 0;
    virtual void setShortSkip(trackedit::secs_t seconds) = 0;
    virtual muse::async::Notification shortSkipChanged() const = 0;

    virtual trackedit::secs_t longSkip() const = 0;
    virtual void setLongSkip(trackedit::secs_t seconds) = 0;
    virtual muse::async::Notification longSkipChanged() const = 0;
};
}
