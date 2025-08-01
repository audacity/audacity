/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3interactiontypes.h"

#include "../../itrackandclipoperations.h"
#include "../../iselectioncontroller.h"
#include "../../itrackeditconfiguration.h"
#include "context/iglobalcontext.h"
#include "iinteractive.h"
#include "modularity/ioc.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
class Au3TrackData;
using Au3TrackDataPtr = std::shared_ptr<Au3TrackData>;

class Au3Interaction : public ITrackAndClipOperations
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<au::trackedit::ITrackeditConfiguration> configuration;

public:
    Au3Interaction();

    muse::secs_t clipStartTime(const trackedit::ClipKey& clipKey) const override;
    muse::secs_t clipEndTime(const trackedit::ClipKey& clipKey) const override;

    bool changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed) override;
    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const override;

    bool trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) override;
    bool silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) override;
    bool changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title) override;

    bool changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle) override;
    bool changeClipPitch(const ClipKey& clipKey, int pitch) override;
    bool resetClipPitch(const ClipKey& clipKey) override;
    bool changeClipSpeed(const ClipKey& clipKey, double speed) override;
    bool resetClipSpeed(const ClipKey& clipKey) override;
    bool changeClipColor(const ClipKey& clipKey, const std::string& color) override;
    bool changeTracksColor(const TrackIdList& tracksIds, const std::string& color) override;
    bool changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize) override;
    bool renderClipPitchAndSpeed(const ClipKey& clipKey) override;
    muse::Ret paste(const std::vector<ITrackDataPtr>& data, secs_t begin, bool moveClips, bool moveAllTracks, bool isMultiSelectionCopy,
                    bool& projectWasModified) override;
    ITrackDataPtr cutClip(const ClipKey& clipKey) override;
    ITrackDataPtr cutTrackData(const TrackId trackId, secs_t begin, secs_t end, bool moveClips) override;
    ITrackDataPtr copyClip(const trackedit::ClipKey& clipKey) override;
    ITrackDataPtr copyNonContinuousTrackData(const TrackId trackId, const ClipKeyList& clipKeys, secs_t offset) override;
    ITrackDataPtr copyContinuousTrackData(const TrackId trackId, secs_t begin, secs_t end) override;
    std::optional<TimeSpan> removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClips(const trackedit::ClipKeyList& clipKeyList, bool moveClips) override;
    bool removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips) override;
    bool moveClips(secs_t timePositionOffset, int trackPositionOffset, bool completed, bool& clipsMovedToOtherTracks) override;
    bool splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots) override;
    bool splitClipsAtSilences(const ClipKeyList& clipKeyList) override;
    bool splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool splitClipsIntoNewTracks(const ClipKeyList& clipKeyList) override;
    bool mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool duplicateClip(const ClipKey& clipKey) override;
    bool duplicateClips(const ClipKeyList& clipKeyList) override;
    ITrackDataPtr clipSplitCut(const ClipKey& clipKey) override;
    bool clipSplitDelete(const ClipKey& clipKey) override;
    std::vector<ITrackDataPtr> splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) override;
    bool splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) override;
    bool trimClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    bool trimClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    bool stretchClipLeft(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    bool stretchClipRight(const ClipKey& clipKey, secs_t deltaSec, secs_t minClipDuration, bool completed) override;
    muse::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;
    std::optional<secs_t> getLeftmostClipStartTime(const ClipKeyList& clipKeys) const override;

    bool newMonoTrack() override;
    bool newStereoTrack() override;
    bool newLabelTrack() override;
    bool deleteTracks(const TrackIdList& trackIds) override;
    bool duplicateTracks(const TrackIdList& trackIds) override;
    bool moveTracks(const TrackIdList& trackIds, const TrackMoveDirection direction) override;
    bool moveTracksTo(const TrackIdList& trackIds, int to) override;

    bool insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration) override;

    bool toggleStretchToMatchProjectTempo(const ClipKey& clipKey) override;

    int64_t clipGroupId(const trackedit::ClipKey& clipKey) const override;
    void setClipGroupId(const trackedit::ClipKey& clipKey, int64_t id) override;
    void groupClips(const trackedit::ClipKeyList& clipKeyList) override;
    void ungroupClips(const trackedit::ClipKeyList& clipKeyList) override;
    ClipKeyList clipsInGroup(int64_t id) const override;

    bool changeTracksFormat(const TrackIdList& tracksIds, trackedit::TrackFormat format) override;
    bool changeTracksRate(const TrackIdList& tracksIds, int rate) override;

    bool swapStereoChannels(const TrackIdList& tracksIds) override;
    bool splitStereoTracksToLRMono(const TrackIdList& tracksIds) override;
    bool splitStereoTracksToCenterMono(const TrackIdList& tracksIds) override;
    bool makeStereoTrack(const TrackId left, const TrackId right) override;
    bool resampleTracks(const TrackIdList& tracksIds, int rate) override;

    muse::ProgressPtr progress() const override;

private:
    friend class Au3InteractionTests;

    int64_t determineNewGroupId(const ClipKeyList& clipKeyList) const;

    au3::Au3Project& projectRef() const;
    void addWaveTrack(int nChannels);
    TrackIdList pasteIntoNewTracks(const std::vector<Au3TrackDataPtr>& tracksData);
    std::shared_ptr<au3::Au3Track> createNewTrackAndPaste(std::shared_ptr<au3::Au3Track> data, au3::Au3TrackList& list, secs_t begin);
    TrackIdList determineDestinationTracksIds(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                              size_t clipboardTracksSize) const;
    TrackIdList expandDestinationTracks(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                        size_t clipboardTracksSize) const;
    NeedsDownmixing moveSelectedClipsUpOrDown(int offset);
    bool clipTransferNeedsDownmixing(const std::vector<Au3TrackDataPtr>& srcTracks, const TrackIdList& dstTracks) const;
    bool userIsOkWithDownmixing() const;
    bool userIsOkCombineMonoToStereo() const;
    bool canMergeMonoTracksToStereo(const TrackId left, const TrackId right);
    muse::Ret canPasteTrackData(const TrackIdList& tracksIds, const std::vector<Au3TrackDataPtr>& clipsToPaste) const;
    muse::Ret makeRoomForClip(const trackedit::ClipKey& clipKey);
    muse::Ret makeRoomForClipsOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<Au3TrackDataPtr>& trackData, secs_t begin);
    muse::Ret makeRoomForDataOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    muse::Ret makeRoomForDataOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<Au3TrackDataPtr>& trackData, secs_t begin,
                                      bool pasteIntoExistingClip);
    bool singleClipOnTrack(WaveTrack* waveTrack) const;
    void trimOrDeleteOverlapping(WaveTrack* waveTrack, secs_t begin, secs_t end, std::shared_ptr<WaveClip> otherClip);
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
    bool mergeSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    bool duplicateSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    void doInsertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration);
    void insertBlankSpace(const TrackIdList& trackIds, secs_t begin, secs_t duration);
    std::shared_ptr<WaveTrack> createMonoTrack();
    std::shared_ptr<WaveTrack> createStereoTrack();

    ITrackDataPtr splitCutSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    bool splitDeleteSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);

    bool canMoveTrack(const TrackId trackId, const TrackMoveDirection direction);
    int trackPosition(const TrackId trackId);
    void moveTrack(const TrackId trackId, const TrackMoveDirection direction);
    void moveTrackTo(const TrackId trackId, int pos);

    bool doChangeClipSpeed(const ClipKey& clipKey, double speed);

    context::IPlaybackStatePtr playbackState() const;

    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> m_clipStartTimeChanged;

    muse::ProgressPtr m_progress;
    std::atomic<bool> m_busy = false;

    std::optional<TrackListInfo> m_startTracklistInfo;
    bool m_moveClipsNeedsDownmixing = false;
};
}
