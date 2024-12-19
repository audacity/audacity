#pragma once

#include "itrackeditinteraction.h"
#include "playback/iplayback.h"
#include "playback/iplayer.h"
#include "global/types/secs.h"
#include "modularity/ioc.h"

namespace au::trackedit {
class TrackeditInteraction : public ITrackeditInteraction, public muse::Injectable
{
    muse::Inject<au::playback::IPlayback> m_playback;

public:
    TrackeditInteraction(std::unique_ptr<ITrackeditInteraction> interaction);

private:
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
    muse::Ret pasteFromClipboard(secs_t begin) override;
    bool cutClipIntoClipboard(const ClipKey& clipKey) override;
    bool cutClipDataIntoClipboard(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool copyClipIntoClipboard(const trackedit::ClipKey& clipKey) override;
    bool copyClipDataIntoClipboard(const trackedit::ClipKey& clipKey, secs_t begin, secs_t end) override;
    bool copyTrackDataIntoClipboard(const TrackId trackId, secs_t begin, secs_t end) override;
    bool removeClip(const trackedit::ClipKey& clipKey) override;
    bool removeTracksData(const TrackIdList& tracksIds, secs_t begin, secs_t end) override;
    bool moveClips(secs_t offset, bool completed) override;
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
    void toggleStretchToMatchProjectTempo(const ClipKey& clipKey) override;
    muse::ProgressPtr progress() const override;

private:
    template<typename Func, typename ... Args>
    auto withPlaybackStop(Func method, Args&&... args)
    {
        m_playback()->player()->stop();
        return (m_interaction.get()->*method)(std::forward<Args>(args)...);
    }

    const std::unique_ptr<ITrackeditInteraction> m_interaction;
};
} // namespace au::trackedit
