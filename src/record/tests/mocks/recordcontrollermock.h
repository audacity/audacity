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

    MOCK_METHOD(const std::vector<trackedit::ClipKey>&, recordingClipKeys, (), (const, override));

    MOCK_METHOD(bool, isMicMeteringOn, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isMicMeteringOnChanged, (), (const, override));

    MOCK_METHOD(bool, isInputMonitoringOn, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isInputMonitoringOnChanged, (), (const, override));

    MOCK_METHOD(bool, isLeadInRecording, (), (const, override));
    MOCK_METHOD(muse::async::Notification, isLeadInRecordingChanged, (), (const, override));
    MOCK_METHOD(muse::secs_t, leadInRecordingStartTime, (), (const, override));
    MOCK_METHOD(std::vector<trackedit::TrackId>, leadInRecordingTrackIds, (), (const, override));
};
}
