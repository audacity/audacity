/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "trackedit/itrackeditproject.h"

#include "UndoManager.h"

#include "iprojecthistory.h"
#include "modularity/ioc.h"

struct TrackListEvent;
namespace au::trackedit {
class Au3TrackeditProject : public ITrackeditProject
{
    muse::Inject<trackedit::IProjectHistory> projectHistory;

public:
    explicit Au3TrackeditProject(const std::shared_ptr<au::au3::IAu3Project>& au3project);
    ~Au3TrackeditProject() override;

    TrackIdList trackIdList() const override;
    std::vector<Track> trackList() const override;
    std::optional<Track> track(TrackId trackId) const override;
    Clip clip(const ClipKey& key) const override;
    muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const override;
    std::vector<int64_t> groupsIdsList() const override;
    std::optional<std::string> trackName(const TrackId& trackId) const override;

    void reload() override;

    void notifyAboutTrackAdded(const Track& track) override;
    void notifyAboutTrackChanged(const Track& track) override;
    void notifyAboutTrackRemoved(const Track& track) override;
    void notifyAboutTrackInserted(const Track& track, int pos) override;
    void notifyAboutTrackMoved(const Track& track, int pos) override;

    void notifyAboutClipChanged(const Clip& clip) override;
    void notifyAboutClipAdded(const Clip& clip) override;
    void notifyAboutClipRemoved(const Clip& clip) override;

    TimeSignature timeSignature() const override;
    void setTimeSignature(const TimeSignature& timeSignature) override;
    muse::async::Channel<TimeSignature> timeSignatureChanged() const override;

    muse::async::Channel<std::vector<au::trackedit::Track> > tracksChanged() const override;
    muse::async::Channel<Track> trackAdded() const override;
    muse::async::Channel<Track> trackChanged() const override;
    muse::async::Channel<Track> trackRemoved() const override;
    muse::async::Channel<Track, int> trackInserted() const override;
    muse::async::Channel<Track, int> trackMoved() const override;

    secs_t totalTime() const override;

    TracksAndClips buildTracksAndClips() const override;

    int64_t createNewGroupID(int64_t startingId = 0) const override;

private:
    void onTrackListEvent(const TrackListEvent& e);
    void onTrackDataChanged(const TrackId& trackId);
    void onProjectTempoChange(double newTempo);

    au::trackedit::Clips getClips(const TrackId& trackId) const;

    struct Au3Impl;
    std::shared_ptr<Au3Impl> m_impl;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip> > m_clipsChanged;
    mutable muse::async::Channel<au::trackedit::TimeSignature> m_timeSignatureChanged;

    mutable muse::async::Channel<trackedit::TrackList> m_tracksChanged;
    mutable muse::async::Channel<trackedit::Track> m_trackAdded;
    mutable muse::async::Channel<trackedit::Track> m_trackChanged;
    mutable muse::async::Channel<trackedit::Track> m_trackRemoved;
    mutable muse::async::Channel<trackedit::Track, int> m_trackInserted;
    mutable muse::async::Channel<trackedit::Track, int> m_trackMoved;
};

class Au3TrackeditProjectCreator : public ITrackeditProjectCreator
{
public:
    Au3TrackeditProjectCreator() = default;

    ITrackeditProjectPtr create(const std::shared_ptr<au::au3::IAu3Project>& au3project) const override;
};
}

struct TimeSignatureRestorer final : UndoStateExtension
{
    explicit TimeSignatureRestorer(AudacityProject& project);
    void RestoreUndoRedoState(AudacityProject& project) override;
    static void reg();

    double mTempo;
    int mUpper;
    int mLower;
};
