/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "record/irecordmetercontroller.h"

namespace au::record {
class RecordMeterControllerMock : public IRecordMeterController
{
public:
    MOCK_METHOD(bool, isRecordMeterVisible, (), (const, override));
    MOCK_METHOD(void, setRecordMeterVisible, (bool), (override));
    MOCK_METHOD(muse::async::Notification, isRecordMeterVisibleChanged, (), (const, override));

    MOCK_METHOD(bool, isSoundActivationMeterVisible, (), (const, override));
    MOCK_METHOD(void, setSoundActivationMeterVisible, (bool), (override));
    MOCK_METHOD(muse::async::Notification, isSoundActivationMeterVisibleChanged, (), (const, override));
};
}
