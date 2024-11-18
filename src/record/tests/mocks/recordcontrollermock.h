/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "record/irecordcontroller.h"

namespace au::record {
class RecordControllerMock : public IRecordController
{
public:
    MOCK_METHOD(bool, isRecordAllowed, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isRecordAllowedChanged, (), (const, override));

    MOCK_METHOD(bool, isRecording, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isRecordingChanged, (), (const, override));
};
}
