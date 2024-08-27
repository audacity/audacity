/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../itrackeditproject.h"

class AudacityProject;
struct TrackListEvent;
namespace au::trackedit {
class Au3TrackeditProject : public ITrackeditProject
{
public:
    Au3TrackeditProject(const std::shared_ptr<au::au3::IAu3Project>& au3project);
    ~Au3TrackeditProject();

    std::vector<TrackId> trackIdList() const override;
    muse::async::NotifyList<Track> trackList() const override;
    Clip clip(const ClipKey& key) const override;
    muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const override;

    void onClipChanged(const Clip& clip) override;
    void onClipAdded(const Clip& clip) override;
    void onClipRemoved(const Clip& clip) override;

    TimeSignature timeSignature() const override;
    void setTimeSignature(const TimeSignature& timeSignature) override;
    muse::async::Channel<TimeSignature> timeSignatureChanged() const override;

    secs_t totalTime() const override;

    void pushHistoryState(const std::string& longDescription, const std::string& shortDescription) override;

private:

    void onTrackListEvent(const TrackListEvent& e);
    void onTrackDataChanged(const TrackId& trackId);

    struct Au3Impl;
    std::shared_ptr<Au3Impl> m_impl;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip>> m_clipsChanged;
    mutable muse::async::ChangedNotifier<trackedit::Track> m_trackChangedNotifier;
    mutable muse::async::Channel<au::trackedit::TimeSignature> m_timeSignatureChanged;
};

class Au3TrackeditProjectCreator : public ITrackeditProjectCreator
{
public:

    Au3TrackeditProjectCreator() = default;

    ITrackeditProjectPtr create(const std::shared_ptr<au::au3::IAu3Project>& au3project) const override;
};
}
