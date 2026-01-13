/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"
#include "trackedit/iselectioncontroller.h"
#include "trackedit/itrackeditconfiguration.h"
#include "trackedit/iprojecthistory.h"
#include "iinteractive.h"
#include "trackedit/itracksinteraction.h"

#include "au3wrap/au3types.h"

#include "au3interactiontypes.h"

#include "trackedit/iclipsinteraction.h"

namespace au::trackedit {
class Au3TrackData;
using Au3TrackDataPtr = std::shared_ptr<Au3TrackData>;

class Au3ClipsInteraction : public IClipsInteraction
{
    muse::GlobalInject<au::trackedit::ITrackeditConfiguration> configuration;

    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<ITracksInteraction> tracksInteraction;

public:
    Au3ClipsInteraction();

    muse::secs_t clipStartTime(const trackedit::ClipKey& clipKey) const override;
    muse::secs_t clipEndTime(const trackedit::ClipKey& clipKey) const override;
    muse::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;

    bool changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed) override;
    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const override;

    bool changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle) override;
    bool changeClipPitch(const ClipKey& clipKey, int pitch) override;
    bool resetClipPitch(const ClipKey& clipKey) override;
    bool changeClipSpeed(const ClipKey& clipKey, double speed) override;
    bool resetClipSpeed(const ClipKey& clipKey) override;
    bool changeClipColor(const ClipKey& clipKey, const std::string& color) override;
    bool changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize) override;
    bool renderClipPitchAndSpeed(const ClipKey& clipKey) override;

    ITrackDataPtr cutClip(const ClipKey& clipKey) override;
    ITrackDataPtr copyClip(const trackedit::ClipKey& clipKey) override;
    std::optional<TimeSpan> removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClips(const trackedit::ClipKeyList& clipKeyList, bool moveClips) override;
    bool moveClips(secs_t timePositionOffset, int trackPositionOffset, bool completed, bool& clipsMovedToOtherTracks) override;
    void cancelClipDragEdit() override;

    bool splitClipsAtSilences(const ClipKeyList& clipKeyList) override;
    bool splitClipsIntoNewTracks(const ClipKeyList& clipKeyList) override;

    bool duplicateClip(const ClipKey& clipKey) override;
    bool duplicateClips(const ClipKeyList& clipKeyList) override;
    ITrackDataPtr clipSplitCut(const ClipKey& clipKey) override;
    bool clipSplitDelete(const ClipKey& clipKey) override;

    bool trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    bool trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    bool stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    bool stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;

    std::optional<secs_t> getLeftmostClipStartTime(const ClipKeyList& clipKeys) const override;
    std::optional<secs_t> getRightmostClipEndTime(const ClipKeyList& clipKeys) const override;
    muse::Ret makeRoomForClip(const trackedit::ClipKey& clipKey) override;

    ClipKeyList clipsOnTrack(const trackedit::TrackId trackId) override;

    bool toggleStretchToMatchProjectTempo(const ClipKey& clipKey) override;

    int64_t clipGroupId(const trackedit::ClipKey& clipKey) const override;
    void setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id) override;
    void groupClips(const trackedit::ClipKeyList& clipKeyList) override;
    void ungroupClips(const trackedit::ClipKeyList& clipKeyList) override;
    ClipKeyList clipsInGroup(int64_t id) const override;

    muse::Progress progress() const override;

    //! TODO
    bool clipTransferNeedsDownmixing(const std::vector<ITrackDataPtr>& srcTracks, const TrackIdList& dstTracks) const override;
    bool userIsOkWithDownmixing() const override;
    muse::Ret makeRoomForClipsOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<ITrackDataPtr>& trackData,
                                       muse::secs_t begin) override;
    muse::Ret makeRoomForDataOnTrack(const TrackId trackId, muse::secs_t begin, muse::secs_t end) override;
    bool singleClipOnTrack(const TrackId trackId) const override;

private:
    friend class Au3ClipsInteractionTests;

    int64_t determineNewGroupId(const ClipKeyList& clipKeyList) const;

    au3::Au3Project& projectRef() const;

    NeedsDownmixing moveSelectedClipsUpOrDown(int offset);

    void trimOrDeleteOverlapping(::WaveTrack* waveTrack, muse::secs_t begin, muse::secs_t end, std::shared_ptr<::WaveClip> otherClip);

    std::optional<secs_t> shortestClipDuration(const ClipKeyList& clipKeys) const;
    bool anyLeftFullyUntrimmed(const ClipKeyList& clipKeys) const;
    bool anyRightFullyUntrimmed(const ClipKeyList& clipKeys) const;
    ClipKeyList determineClipsForInteraction(const ClipKey& clipKey) const;
    secs_t clampLeftTrimDelta(const ClipKeyList& clipKeys, secs_t deltaSec, secs_t minClipDuration) const;
    secs_t clampRightTrimDelta(const ClipKeyList& clipKeys, secs_t deltaSec, secs_t minClipDuration) const;
    secs_t clampLeftStretchDelta(const ClipKeyList& clipKeys, secs_t deltaSec, secs_t minClipDuration) const;
    secs_t clampRightStretchDelta(const ClipKeyList& clipKeys, secs_t deltaSec, secs_t minClipDuration) const;
    bool trimClipsLeft(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed);
    bool trimClipsRight(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed);
    bool stretchClipsLeft(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed);
    bool stretchClipsRight(const ClipKeyList& clipKeys, secs_t deltaSec, bool completed);

    bool doChangeClipSpeed(const ClipKey& clipKey, double speed);

    context::IPlaybackStatePtr playbackState() const;

    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> m_clipStartTimeChanged;

    muse::Progress m_progress;
    std::atomic<bool> m_busy = false;

    std::optional<TrackListInfo> m_tracksWhenDragStarted;
    bool m_moveClipsNeedsDownmixing = false;
};
}
