/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/itrackeditclipboard.h"

namespace au::trackedit {
class ClipboardMock : public ITrackeditClipboard
{
public:

    MOCK_METHOD(std::vector<ITrackDataPtr>, trackDataCopy, (), (const, override));
    MOCK_METHOD(void, clearTrackData, (), (override));
    MOCK_METHOD(bool, trackDataEmpty, (), (const, override));
    MOCK_METHOD(size_t, trackDataSize, (), (const, override));
    MOCK_METHOD(void, addTrackData, (ITrackDataPtr), (override));

    MOCK_METHOD(void, setMultiSelectionCopy, (bool newValue), (override));
    MOCK_METHOD(bool, isMultiSelectionCopy, (), (const, override));
};
}
