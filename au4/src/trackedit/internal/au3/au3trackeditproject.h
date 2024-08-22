/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "../../itrackeditproject.h"

#include "modularity/ioc.h"
#include "context/iglobalcontext.h"

class AudacityProject;
namespace au::trackedit {
class Au3TrackeditProject : public ITrackeditProject
{
    muse::Inject<au::context::IGlobalContext> globalContext;

public:
    Au3TrackeditProject() = default;

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

    AudacityProject* au3ProjectPtr() const;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip>> m_clipsChanged;
    mutable muse::async::ChangedNotifier<trackedit::Track> m_trackChangedNotifier;
    mutable muse::async::Channel<au::trackedit::TimeSignature> m_timeSignatureChanged;
};

class Au3TrackeditProjectCreator : public ITrackeditProjectCreator
{
public:

    Au3TrackeditProjectCreator() = default;

    ITrackeditProjectPtr create() const override;
};
}
