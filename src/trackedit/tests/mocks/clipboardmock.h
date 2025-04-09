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

    MOCK_METHOD(std::vector<TrackData>, trackDataSource, (), (const, override));
    MOCK_METHOD(std::vector<TrackData>, trackDataCopy, (), (const, override));
    MOCK_METHOD(TrackData, trackData, (size_t i), (const, override));
    MOCK_METHOD(void, clearTrackData, (), (override));
    MOCK_METHOD(bool, trackDataEmpty, (), (const, override));
    MOCK_METHOD(size_t, trackDataSize, (), (const, override));
    MOCK_METHOD(void, addTrackData, (const TrackData& trackData), (override));

    MOCK_METHOD(void, setMultiSelectionCopy, (bool newValue), (override));
    MOCK_METHOD(bool, isMultiSelectionCopy, (), (const, override));
};
}
