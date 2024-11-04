/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../itrackeditinteraction.h"

#include "Track.h"
#include "iinteractive.h"
#include "iprojecthistory.h"
#include "iselectioncontroller.h"
#include "itrackeditclipboard.h"
#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

#include "au3wrap/au3types.h"

namespace au::trackedit {
class Au3Interaction : public ITrackeditInteraction
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<au::trackedit::ITrackeditClipboard> clipboard;

public:
    Au3Interaction();

    muse::secs_t clipStartTime(const trackedit::ClipKey& clipKey) const override;

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
    bool changeClipOptimizeForVoice(const ClipKey& clipKey, bool optimize) override;
    void renderClipPitchAndSpeed(const ClipKey& clipKey) override;
    void clearClipboard() override;
    muse::Ret pasteFromClipboard(secs_t begin, trackedit::TrackId trackId) override;
    bool cutClipIntoClipboard(const ClipKey& clipKey) override;
    bool cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool copyClipIntoClipboard(const trackedit::ClipKey& clipKey) override;
    bool copyClipDataIntoClipboard(const trackedit::ClipKey& clipKey, secs_t begin, secs_t end) override;
    bool copyTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end) override;
    bool removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClipsData(const std::vector<trackedit::ClipKey>& clipsKeys, secs_t begin, secs_t end) override;
    bool splitTracksAt(const TrackIdList& tracksIds, secs_t pivot) override;
    bool mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool duplicateClip(const ClipKey& clipKey) override;
    bool clipSplitCut(const ClipKey& clipKey) override;
    bool clipSplitDelete(const ClipKey& clipKey) override;
    bool splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) override;
    bool splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) override;
    bool trimClipLeft(const trackedit::ClipKey& clipKey, secs_t deltaSec, bool completed) override;
    bool trimClipRight(const trackedit::ClipKey& clipKey, secs_t deltaSec, bool completed) override;
    muse::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;

    void newMonoTrack() override;
    void newStereoTrack() override;
    void newLabelTrack() override;
    void deleteTracks(const TrackIdList& trackIds) override;
    void duplicateTracks(const TrackIdList& trackIds) override;
    void moveTracks(const TrackIdList& trackIds, const TrackMoveDirection direction) override;
    void moveTracksTo(const TrackIdList& trackIds, int to) override;

    void undo() override;
    bool canUndo() override;
    void redo() override;
    bool canRedo() override;

    muse::ProgressPtr progress() const override;

private:
    au3::Au3Project& projectRef() const;
    TrackIdList pasteIntoNewTracks();
    au3::Au3Track::Holder createNewTrackAndPaste(std::shared_ptr<au3::Au3Track> data, au3::Au3TrackList& list, secs_t begin);
    TrackIdList determineDestinationTracksIds(const std::vector<Track>& tracks, TrackId destinationTrackId, size_t tracksNum) const;
    muse::Ret canPasteClips(const TrackIdList& tracksIds,  secs_t begin) const;
    bool cutTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end);
    bool mergeSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    bool duplicateSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);

    bool splitCutSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    bool splitDeleteSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);

    void pushProjectHistoryJoinState(secs_t start, secs_t duration);
    void pushProjectHistorySplitDeleteState(secs_t start, secs_t duration);
    void pushProjectHistoryDuplicateState();

    void pushProjectHistoryTrackAddedState();
    void pushProjectHistoryTracksTrimState(secs_t start, secs_t end);
    void pushProjectHistoryTrackSilenceState(secs_t start, secs_t end);
    void pushProjectHistoryPasteState();
    void pushProjectHistoryDeleteState(secs_t start, secs_t duration);
    void pushProjectHistoryChangeClipPitchState();
    void pushProjectHistoryResetClipPitchState();
    void pushProjectHistoryChangeClipSpeedState();
    void pushProjectHistoryResetClipSpeedState();
    void pushProjectHistoryRenderClipStretchingState();

    bool canMoveTrack(const TrackId trackId, const TrackMoveDirection direction);
    int trackPosition(const TrackId trackId);
    void moveTrack(const TrackId trackId, const TrackMoveDirection direction);
    void moveTrackTo(const TrackId trackId, int pos);

    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> m_clipStartTimeChanged;

    muse::ProgressPtr m_progress;
};
}
