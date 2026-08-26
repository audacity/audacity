/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <gmock/gmock.h>

#include "trackedit/iclipsinteraction.h"

namespace au::trackedit {
class ClipsInteractionMock : public IClipsInteraction
{
public:
    MOCK_METHOD(secs_t, clipStartTime, (const ClipKey&), (const, override));
    MOCK_METHOD(secs_t, clipEndTime, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(secs_t, clipDuration, (const ClipKey&), (const, override));

    MOCK_METHOD(bool, changeClipStartTime, (const ClipKey&, secs_t, bool), (override));
    MOCK_METHOD(bool, changeClipsStartTime, (const ClipKeyList&, secs_t, bool), (override));
    MOCK_METHOD((muse::async::Channel<ClipKey, secs_t, bool>), clipStartTimeChanged, (), (const, override));

    MOCK_METHOD(bool, changeClipTitle, (const ClipKey&, const muse::String&), (override));
    MOCK_METHOD(bool, changeClipPitch, (const ClipKey&, int), (override));
    MOCK_METHOD(bool, resetClipPitch, (const ClipKey&), (override));
    MOCK_METHOD(bool, changeClipSpeed, (const ClipKey&, double), (override));
    MOCK_METHOD(bool, resetClipSpeed, (const ClipKey&), (override));
    MOCK_METHOD(bool, changeClipColor, (const ClipKey&, ClipColorIndex), (override));
    MOCK_METHOD(bool, changeClipOptimizeForVoice, (const ClipKey&, bool), (override));
    MOCK_METHOD(bool, renderClipPitchAndSpeed, (const ClipKey&), (override));

    MOCK_METHOD(ITrackDataPtr, cutClip, (const ClipKey&), (override));
    MOCK_METHOD(ITrackDataPtr, copyClip, (const ClipKey&), (override));
    MOCK_METHOD(std::optional<TimeSpan>, removeClip, (const ClipKey&), (override));
    MOCK_METHOD(bool, removeClips, (const ClipKeyList&, bool), (override));
    MOCK_METHOD(muse::RetVal<ClipKeyList>, moveClips, (const ClipKeyList&, secs_t, int, bool, bool&), (override));
    MOCK_METHOD(void, cancelClipDragEdit, (), (override));

    MOCK_METHOD(bool, splitClipsAtSilences, (const ClipKeyList&), (override));
    MOCK_METHOD(bool, splitClipsIntoNewTracks, (const ClipKeyList&), (override));

    MOCK_METHOD(bool, duplicateClip, (const ClipKey&), (override));
    MOCK_METHOD(bool, duplicateClips, (const ClipKeyList&), (override));
    MOCK_METHOD(ITrackDataPtr, clipSplitCut, (const ClipKey&), (override));
    MOCK_METHOD(bool, clipSplitDelete, (const ClipKey&), (override));

    MOCK_METHOD(bool, trimClipsLeft, (const ClipKeyList&, secs_t, secs_t, bool), (override));
    MOCK_METHOD(bool, trimClipsRight, (const ClipKeyList&, secs_t, secs_t, bool), (override));

    MOCK_METHOD(bool, stretchClipsLeft, (const ClipKeyList&, secs_t, secs_t, bool), (override));
    MOCK_METHOD(bool, stretchClipsRight, (const ClipKeyList&, secs_t, secs_t, bool), (override));

    MOCK_METHOD(muse::Ret, makeRoomForClip, (const trackedit::ClipKey&), (override));
    MOCK_METHOD(muse::Ret, makeRoomForClips, (const ClipKeyList&), (override));

    MOCK_METHOD(ClipKeyList, clipsOnTrack, (const trackedit::TrackId), (override));

    MOCK_METHOD(bool, toggleStretchToMatchProjectTempo, (const ClipKey&), (override));

    MOCK_METHOD(int64_t, clipGroupId, (const trackedit::ClipKey&), (const, override));
    MOCK_METHOD(void, setClipGroupId, (const trackedit::ClipKey&, int64_t), (override));
    MOCK_METHOD(void, groupClips, (const trackedit::ClipKeyList&), (override));
    MOCK_METHOD(void, ungroupClips, (const trackedit::ClipKeyList&), (override));
    MOCK_METHOD(ClipKeyList, clipsInGroup, (int64_t), (const, override));

    MOCK_METHOD(muse::Progress, progress, (), (const, override));

    MOCK_METHOD(bool, clipTransferNeedsDownmixing, (const std::vector<ITrackDataPtr>&, const TrackIdList&), (const, override));
    MOCK_METHOD(bool, userIsOkWithDownmixing, (), (const, override));
};
}
