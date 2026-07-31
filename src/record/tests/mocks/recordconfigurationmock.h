/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "record/irecordconfiguration.h"

namespace au::record {
class RecordConfigurationMock : public IRecordConfiguration
{
public:
    MOCK_METHOD(bool, isMicMeteringOn, (), (const, override));
    MOCK_METHOD(void, setIsMicMeteringOn, (bool), (override));
    MOCK_METHOD(muse::async::Notification, isMicMeteringOnChanged, (), (const, override));

    MOCK_METHOD(bool, isInputMonitoringOn, (), (const, override));
    MOCK_METHOD(void, setIsInputMonitoringOn, (bool), (override));
    MOCK_METHOD(muse::async::Notification, isInputMonitoringOnChanged, (), (const, override));

    MOCK_METHOD(double, leadInTimeDuration, (), (const, override));
    MOCK_METHOD(void, setLeadInTimeDuration, (double), (override));
    MOCK_METHOD(muse::async::Notification, leadInTimeDurationChanged, (), (const, override));

    MOCK_METHOD(double, crossfadeDuration, (), (const, override));
    MOCK_METHOD(void, setCrossfadeDuration, (double), (override));
    MOCK_METHOD(muse::async::Notification, crossfadeDurationChanged, (), (const, override));
};
}
