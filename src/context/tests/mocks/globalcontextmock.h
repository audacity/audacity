/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "context/iglobalcontext.h"

namespace au::context {
class GlobalContextMock : public IGlobalContext
{
public:
    MOCK_METHOD(void, setCurrentProject, (const project::IAudacityProjectPtr&), (override));
    MOCK_METHOD(project::IAudacityProjectPtr, currentProject, (), (const, override));
    MOCK_METHOD(muse::async::Notification, currentProjectChanged, (), (const, override));

    MOCK_METHOD(trackedit::ITrackeditProjectPtr, currentTrackeditProject, (), (const, override));
    MOCK_METHOD(muse::async::Notification, currentTrackeditProjectChanged, (), (const, override));

    MOCK_METHOD(void, setPlayer, (const playback::IPlayerPtr&), (override));
    MOCK_METHOD(IPlaybackStatePtr, playbackState, (), (const, override));

    MOCK_METHOD(bool, isRecording, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isRecordingChanged, (), (const, override));
    MOCK_METHOD(muse::secs_t, recordPosition, (), (const, override));
    MOCK_METHOD(muse::async::Channel<muse::secs_t>, recordPositionChanged, (), (const, override));
};
}
