/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "playback/iplayback.h"

namespace au::playback {
class PlaybackMock : public IPlayback
{
public:
    MOCK_METHOD(std::shared_ptr<IPlayer>, player, (TrackSequenceId), (const, override));
    MOCK_METHOD(std::shared_ptr<IAudioOutput>, audioOutput, (), (const, override));
};
}
