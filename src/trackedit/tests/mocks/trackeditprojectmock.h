/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <gmock/gmock.h>

#include "trackedit/itrackeditproject.h"

namespace au::trackedit {
class TrackeditProjectMock : public ITrackeditProject
{
public:
    MOCK_METHOD(std::vector<TrackId>, trackIdList, (), (const, override));
    MOCK_METHOD(std::vector<Track>, trackList, (), (const, override));
    MOCK_METHOD(Clip, clip, (const ClipKey& key), (const, override));
    MOCK_METHOD(muse::async::NotifyList<Clip>, clipList, (const TrackId& trackId), (const, override));
    MOCK_METHOD(std::vector<int64_t>, groupsIdsList, (), (const, override));
    MOCK_METHOD(std::optional<std::string>, trackName, (const TrackId& trackId), (const, override));

    MOCK_METHOD(void, reload, (), (override));

    MOCK_METHOD(void, notifyAboutTrackAdded, (const Track& track), (override));
    MOCK_METHOD(void, notifyAboutTrackChanged, (const Track& track), (override));
    MOCK_METHOD(void, notifyAboutTrackRemoved, (const Track& track), (override));
    MOCK_METHOD(void, notifyAboutTrackInserted, (const Track& track, int pos), (override));
    MOCK_METHOD(void, notifyAboutTrackMoved, (const Track& track, int pos), (override));

    MOCK_METHOD(void, notifyAboutClipChanged, (const Clip& clip), (override));
    MOCK_METHOD(void, notifyAboutClipAdded, (const Clip& clip), (override));
    MOCK_METHOD(void, notifyAboutClipRemoved, (const Clip& clip), (override));

    MOCK_METHOD(TimeSignature, timeSignature, (), (const, override));
    MOCK_METHOD(void, setTimeSignature, (const TimeSignature& timeSignature), (override));
    MOCK_METHOD(muse::async::Channel<TimeSignature>, timeSignatureChanged, (), (const, override));

    MOCK_METHOD(muse::async::Channel<std::vector<au::trackedit::Track> >, tracksChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<trackedit::Track>, trackAdded, (), (const, override));
    MOCK_METHOD(muse::async::Channel<trackedit::Track>, trackChanged, (), (const, override));
    MOCK_METHOD(muse::async::Channel<trackedit::Track>, trackRemoved, (), (const, override));
    MOCK_METHOD((muse::async::Channel<trackedit::Track, int>), trackInserted, (), (const, override));
    MOCK_METHOD((muse::async::Channel<trackedit::Track, int>), trackMoved, (), (const, override));

    MOCK_METHOD(secs_t, totalTime, (), (const, override));
};
}
