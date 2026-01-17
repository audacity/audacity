/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include <gmock/gmock.h>

#include "playback/iplaybackconfiguration.h"

namespace au::playback {
class PlaybackConfigurationMock : public IPlaybackConfiguration
{
public:
    MOCK_METHOD(au::uicomponents::TimecodeFormatType, playbackTimeItemFormat, (), (const, override));
    MOCK_METHOD(void, setPlaybackTimeItemFormat, (au::uicomponents::TimecodeFormatType format), (override));
    MOCK_METHOD(muse::async::Notification, playbackTimeItemFormatChanged, (), (const, override));

    MOCK_METHOD(PlaybackMeterStyle::MeterStyle, playbackMeterStyle, (), (const, override));
    MOCK_METHOD(void, setPlaybackMeterStyle, (PlaybackMeterStyle::MeterStyle style), (override));
    MOCK_METHOD(muse::async::Notification, playbackMeterStyleChanged, (), (const, override));

    MOCK_METHOD(PlaybackMeterType::MeterType, playbackMeterType, (), (const, override));
    MOCK_METHOD(void, setPlaybackMeterType, (PlaybackMeterType::MeterType type), (override));
    MOCK_METHOD(muse::async::Notification, playbackMeterTypeChanged, (), (const, override));

    MOCK_METHOD(PlaybackMeterPosition::MeterPosition, playbackMeterPosition, (), (const, override));
    MOCK_METHOD(void, setPlaybackMeterPosition, (PlaybackMeterPosition::MeterPosition position), (override));
    MOCK_METHOD(muse::async::Notification, playbackMeterPositionChanged, (), (const, override));

    MOCK_METHOD(PlaybackMeterDbRange::DbRange, playbackMeterDbRange, (), (const, override));
    MOCK_METHOD(void, setPlaybackMeterDbRange, (PlaybackMeterDbRange::DbRange range), (override));
    MOCK_METHOD(muse::async::Notification, playbackMeterDbRangeChanged, (), (const, override));

    MOCK_METHOD(int, playbackHorizontalMeterSize, (), (const, override));
    MOCK_METHOD(void, setPlaybackHorizontalMeterSize, (int size), (override));
    MOCK_METHOD(muse::async::Notification, playbackHorizontalMeterSizeChanged, (), (const, override));

    MOCK_METHOD(std::vector<PlaybackQualityPrefs::PlaybackQuality>, playbackQualityList, (), (const, override));
    MOCK_METHOD(PlaybackQualityPrefs::PlaybackQuality, currentPlaybackQuality, (), (const, override));
    MOCK_METHOD(void, setPlaybackQuality, (PlaybackQualityPrefs::PlaybackQuality quality), (override));
    MOCK_METHOD(muse::async::Notification, playbackQualityChanged, (), (const, override));

    MOCK_METHOD(std::vector<DitherTypePrefs::DitherType>, ditheringList, (), (const, override));
    MOCK_METHOD(DitherTypePrefs::DitherType, currentDithering, (), (const, override));
    MOCK_METHOD(void, setDithering, (DitherTypePrefs::DitherType dithering), (override));
    MOCK_METHOD(muse::async::Notification, ditheringChanged, (), (const, override));

    MOCK_METHOD(TracksBehaviors::SoloBehavior, currentSoloBehavior, (), (const, override));
    MOCK_METHOD(void, setSoloBehavior, (TracksBehaviors::SoloBehavior behavior), (override));
    MOCK_METHOD(muse::async::Notification, soloBehaviorChanged, (), (const, override));

    MOCK_METHOD(trackedit::secs_t, shortSkip, (), (const, override));
    MOCK_METHOD(void, setShortSkip, (trackedit::secs_t seconds), (override));
    MOCK_METHOD(muse::async::Notification, shortSkipChanged, (), (const, override));

    MOCK_METHOD(trackedit::secs_t, longSkip, (), (const, override));
    MOCK_METHOD(void, setLongSkip, (trackedit::secs_t seconds), (override));
    MOCK_METHOD(muse::async::Notification, longSkipChanged, (), (const, override));

    MOCK_METHOD(bool, selectionFollowsLoopRegion, (), (const, override));
    MOCK_METHOD(void, setSelectionFollowsLoopRegion, (bool follows), (override));
    MOCK_METHOD(muse::async::Notification, selectionFollowsLoopRegionChanged, (), (const, override));
};
}
