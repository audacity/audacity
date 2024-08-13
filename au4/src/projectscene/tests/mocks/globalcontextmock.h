#pragma once

#include "context/iglobalcontext.h"
#include <gmock/gmock.h>

namespace au::projectscene {
class GlobalContextMock : public context::IGlobalContext
{
public:
    MOCK_METHOD(au::trackedit::ITrackeditProjectPtr, currentTrackeditProject, (), (const, override));
    MOCK_METHOD(void, setCurrentProject, (const au::project::IAudacityProjectPtr&), (override));
    MOCK_METHOD(au::project::IAudacityProjectPtr, currentProject, (), (const, override));
    MOCK_METHOD(muse::async::Notification, currentProjectChanged, (), (const, override));
    MOCK_METHOD(muse::async::Notification, currentTrackeditProjectChanged, (), (const, override));
    MOCK_METHOD(void, setPlayer, (const au::playback::IPlayerPtr&), (override));
    MOCK_METHOD(context::IPlaybackStatePtr, playbackState, (), (const, override));
};
}
