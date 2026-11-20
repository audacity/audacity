/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "macromanager/imacrorunner.h"

namespace au::macromanager {
class MacroRunnerMock : public IMacroRunner
{
public:
    MOCK_METHOD(muse::Ret, validate, (const Macro&), (const, override));
    MOCK_METHOD(muse::async::Promise<muse::Ret>, run, (const Macro&), (override));
    MOCK_METHOD(muse::async::Promise<muse::Ret>, runOnFiles, (const Macro&, const muse::io::paths_t&), (override));
    MOCK_METHOD(void, abort, (), (override));

    MOCK_METHOD(bool, isRunning, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isRunningChanged, (), (const, override));

    MOCK_METHOD(size_t, currentFileIndex, (), (const, override));
    MOCK_METHOD(std::optional<std::string>, currentFileName, (), (const, override));
    MOCK_METHOD(std::vector<muse::Ret>, fileResults, (), (const, override));
    MOCK_METHOD(muse::async::Notification, currentFileChanged, (), (const, override));
};
}
