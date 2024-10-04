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

class AudacityProject;

namespace au::trackedit {
class Au3Interaction : public ITrackeditInteraction
{
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<au::trackedit::ITrackeditClipboard> clipboard;

public:
    Au3Interaction() = default;

    audio::secs_t clipStartTime(const trackedit::ClipKey& clipKey) const override;

    bool changeClipStartTime(const trackedit::ClipKey& clipKey, secs_t newStartTime, bool completed) override;
    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> clipStartTimeChanged() const override;

    bool trimTrackData(trackedit::TrackId trackId, secs_t begin, secs_t end) override;
    bool silenceTrackData(trackedit::TrackId trackId, secs_t begin, secs_t end) override;

    bool changeClipTitle(const trackedit::ClipKey& clipKey, const muse::String& newTitle) override;
    void clearClipboard() override;
    muse::Ret pasteFromClipboard(secs_t begin, trackedit::TrackId trackId) override;
    bool cutClipIntoClipboard(const ClipKey& clipKey) override;
    bool cutClipDataIntoClipboard(const std::vector<TrackId>& tracksIds, secs_t begin, secs_t end) override;
    bool copyClipIntoClipboard(const trackedit::ClipKey& clipKey) override;
    bool copyClipDataIntoClipboard(const trackedit::ClipKey& clipKey, secs_t begin, secs_t end) override;
    bool copyTrackDataIntoClipboard(const trackedit::TrackId trackId, secs_t begin, secs_t end) override;
    bool removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeClipData(const trackedit::ClipKey& clipKey, secs_t begin, secs_t end) override;
    bool splitAt(const TrackId trackId, secs_t pivot) override;
    bool mergeSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end) override;
    bool duplicateSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end) override;
    bool duplicateClip(const ClipKey& clipKey) override;
    bool clipSplitCut(const ClipKey& clipKey) override;
    bool clipSplitDelete(const ClipKey& clipKey) override;
    bool splitCutSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end) override;
    bool splitDeleteSelectedOnTracks(const std::vector<TrackId> tracksIds, secs_t begin, secs_t end) override;
    bool trimClipLeft(const trackedit::ClipKey& clipKey, secs_t deltaSec, bool completed) override;
    bool trimClipRight(const trackedit::ClipKey& clipKey, secs_t deltaSec, bool completed) override;
    void newMonoTrack() override;
    void newStereoTrack() override;
    void newLabelTrack() override;
    audio::secs_t clipDuration(const trackedit::ClipKey& clipKey) const override;
    void undo() override;
    void redo() override;

private:
    AudacityProject& projectRef() const;
    muse::Ret pasteIntoNewTrack();
    ::Track::Holder createNewTrackAndPaste(std::shared_ptr<::Track> data, ::TrackList &list, secs_t begin);
    std::vector<TrackId> determineDestinationTracksIds(const std::vector<Track>& tracks,
                                    TrackId destinationTrackId, size_t tracksNum) const;
    muse::Ret canPasteClips(const std::vector<TrackId>& tracksIds,  secs_t begin) const;
    bool cutTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end);
    bool mergeSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    bool duplicateSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);

    bool splitCutSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);
    bool splitDeleteSelectedOnTrack(const TrackId trackId, secs_t begin, secs_t end);

    void pushProjectHistoryJoinState(secs_t start, secs_t duration);
    void pushProjectHistorySplitDeleteState(secs_t start, secs_t duration);
    void pushProjectHistoryDuplicateState();

    muse::async::Channel<trackedit::ClipKey, secs_t /*newStartTime*/, bool /*completed*/> m_clipStartTimeChanged;
};
}
