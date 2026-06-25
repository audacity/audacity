/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "record/irecord.h"

namespace au::record {
class RecordMock : public IRecord
{
public:
    MOCK_METHOD(muse::Ret, start, (), (override));
    MOCK_METHOD(muse::Ret, pause, (), (override));
    MOCK_METHOD(muse::Ret, stop, (), (override));
    MOCK_METHOD(muse::Ret, leadInRecording, (), (override));

    MOCK_METHOD(IAudioInputPtr, audioInput, (), (const, override));

    MOCK_METHOD(muse::async::Channel<muse::secs_t>, recordPositionChanged, (), (const, override));

    MOCK_METHOD(const std::vector<trackedit::ClipKey>&, recordingClipKeys, (), (const, override));

    MOCK_METHOD(muse::async::Notification, recordingFinished, (), (const, override));
};
}
