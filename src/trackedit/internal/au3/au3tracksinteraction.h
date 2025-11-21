/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3wrap/au3types.h"

#include "modularity/ioc.h"
#include "iinteractive.h"
#include "context/iglobalcontext.h"
#include "../../itrackeditconfiguration.h"
#include "../../iclipsinteraction.h"
#include "../../iselectioncontroller.h"
#include "../../iprojecthistory.h"

#include "../../itracksinteraction.h"

namespace au::trackedit {
class Au3TrackData;
using Au3TrackDataPtr = std::shared_ptr<Au3TrackData>;

class Au3TracksInteraction : public ITracksInteraction
{
    muse::Inject<muse::IInteractive> interactive;
    muse::Inject<au::context::IGlobalContext> globalContext;
    muse::Inject<au::trackedit::ISelectionController> selectionController;
    muse::Inject<au::trackedit::ITrackeditConfiguration> configuration;
    muse::Inject<au::trackedit::IProjectHistory> projectHistory;
    muse::Inject<au::trackedit::IClipsInteraction> clipsInteraction;

public:
    Au3TracksInteraction();

    bool trimTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) override;
    bool silenceTracksData(const std::vector<trackedit::TrackId>& tracksIds, secs_t begin, secs_t end) override;
    bool changeTrackTitle(const trackedit::TrackId trackId, const muse::String& title) override;
    bool changeTracksColor(const TrackIdList& tracksIds, const std::string& color) override;
    bool changeTrackRulerType(const trackedit::TrackId& trackId, trackedit::TrackRulerType rulerType) override;
    bool changeAudioTrackViewType(const trackedit::TrackId& trackId, trackedit::TrackViewType viewType) override;

    muse::Ret paste(const std::vector<ITrackDataPtr>& data, secs_t begin, bool moveClips, bool moveAllTracks, bool isMultiSelectionCopy,
                    bool& projectWasModified) override;

    ITrackDataPtr cutTrackData(const TrackId trackId, secs_t begin, secs_t end, bool moveClips) override;
    ITrackDataPtr copyNonContinuousTrackData(const TrackId trackId, const ClipKeyList& clipKeys, secs_t offset) override;
    ITrackDataPtr copyContinuousTrackData(const TrackId trackId, secs_t begin, secs_t end) override;
    bool removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end, bool moveClips) override;

    bool splitTracksAt(const TrackIdList& tracksIds, std::vector<secs_t> pivots) override;
    bool splitRangeSelectionAtSilences(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool splitRangeSelectionIntoNewTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool mergeSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool duplicateSelectedOnTracks(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    std::vector<ITrackDataPtr> splitCutSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) override;
    bool splitDeleteSelectedOnTracks(const TrackIdList tracksIds, secs_t begin, secs_t end) override;

    bool newMonoTrack() override;
    bool newStereoTrack() override;
    bool newLabelTrack() override;
    bool deleteTracks(const TrackIdList& trackIds) override;
    bool duplicateTracks(const TrackIdList& trackIds) override;
    bool moveTracks(const TrackIdList& trackIds, const TrackMoveDirection direction) override;
    bool moveTracksTo(const TrackIdList& trackIds, int to) override;

    bool insertSilence(const TrackIdList& trackIds, secs_t begin, secs_t end, secs_t duration) override;

    bool changeTracksFormat(const TrackIdList& tracksIds, trackedit::TrackFormat format) override;
    bool changeTracksRate(const TrackIdList& tracksIds, int rate) override;

    bool swapStereoChannels(const TrackIdList& tracksIds) override;
    bool splitStereoTracksToLRMono(const TrackIdList& tracksIds) override;
    bool splitStereoTracksToCenterMono(const TrackIdList& tracksIds) override;
    bool makeStereoTrack(const TrackId left, const TrackId right) override;
    bool resampleTracks(const TrackIdList& tracksIds, int rate) override;

    double nearestZeroCrossing(double time) const override;

    muse::Progress progress() const override;

private:
    friend class Au3TracksInteractionTests;

    au3::Au3Project& projectRef() const;
    void addWaveTrack(int nChannels);
    TrackIdList pasteIntoNewTracks(const std::vector<Au3TrackDataPtr>& tracksData);
    std::shared_ptr<au3::Au3Track> createNewTrackAndPaste(std::shared_ptr<au3::Au3Track> data, au3::Au3TrackList& list, secs_t begin);
    TrackIdList determineDestinationTracksIds(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                              const std::vector<Au3TrackDataPtr>& clipboardData, bool forLabels = false) const;
    TrackIdList expandDestinationTracks(const std::vector<Track>& tracks, const TrackIdList& destinationTrackIds,
                                        size_t clipboardTracksSize) const;

    bool userIsOkCombineMonoToStereo() const;
    bool canMergeMonoTracksToStereo(const TrackId left, const TrackId right);
    muse::Ret canPasteTrackData(const TrackIdList& tracksIds, const std::vector<Au3TrackDataPtr>& clipsToPaste) const;

    muse::Ret makeRoomForDataOnTracks(const std::vector<TrackId>& tracksIds, const std::vector<Au3TrackDataPtr>& trackData, secs_t begin,
                                      bool pasteIntoExistingClip);

    muse::Ret pasteClips(const std::vector<Au3TrackDataPtr>& copiedData, const TrackIdList& dstTracksIds, secs_t begin, bool moveClips,
                         bool isMultiSelectionCopy, bool pasteIntoExistingClip, bool& projectWasModified);
    muse::Ret pasteLabels(const std::vector<Au3TrackDataPtr>& copiedData, const TrackIdList& dstTracksIds, secs_t begin, bool moveClips,
                          bool& projectWasModified);

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

    context::IPlaybackStatePtr playbackState() const;

    muse::Progress m_progress;
    std::atomic<bool> m_busy = false;
};
}
