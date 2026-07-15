/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "framework/rcommand/icommanddispatcher.h"

namespace muse::rcommand {
class CommandDispatcherMock : public ICommandDispatcher
{
public:
    MOCK_METHOD(muse::async::Promise<Response>, dispatch, (const Request&), (override));
    MOCK_METHOD(void, onRequest, (Commandable*, const Command&, const CallBack&), (override));
    MOCK_METHOD(void, unreg, (Commandable*), (override));
};
}
