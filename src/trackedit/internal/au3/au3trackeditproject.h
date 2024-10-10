/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../itrackeditproject.h"

struct TrackListEvent;
namespace au::trackedit {
class Au3TrackeditProject : public ITrackeditProject
{
public:
    Au3TrackeditProject(const std::shared_ptr<au::au3::IAu3Project>& au3project);
    ~Au3TrackeditProject();

    TrackIdList trackIdList() const override;
    muse::async::NotifyList<Track> trackList() const override;
    Clip clip(const ClipKey& key) const override;
    muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const override;

    void reload() override;

    void onTrackAdded(const Track& track) override;
    void onTrackChanged(const Track& track) override;
    void onTrackRemoved(const Track& track) override;

    void onClipChanged(const Clip& clip) override;
    void onClipAdded(const Clip& clip) override;
    void onClipRemoved(const Clip& clip) override;

    TimeSignature timeSignature() const override;
    void setTimeSignature(const TimeSignature& timeSignature) override;
    muse::async::Channel<TimeSignature> timeSignatureChanged() const override;

    secs_t totalTime() const override;

private:

    void onTrackListEvent(const TrackListEvent& e);
    void onTrackDataChanged(const TrackId& trackId);

    struct Au3Impl;
    std::shared_ptr<Au3Impl> m_impl;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip>> m_clipsChanged;
    mutable muse::async::ChangedNotifier<trackedit::Track> m_tracksChanged;
    mutable muse::async::Channel<au::trackedit::TimeSignature> m_timeSignatureChanged;
};

class Au3TrackeditProjectCreator : public ITrackeditProjectCreator
{
public:

    Au3TrackeditProjectCreator() = default;

    ITrackeditProjectPtr create(const std::shared_ptr<au::au3::IAu3Project>& au3project) const override;
};
}
