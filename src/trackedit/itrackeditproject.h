/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>
#include <optional>

#include "modularity/imoduleinterface.h"

#include "global/async/notifylist.h"
#include "global/async/channel.h"

#include "trackedittypes.h"
#include "dom/track.h"

namespace au::au3 {
class IAu3Project;
}

namespace au::trackedit {
class ITrackeditProject
{
public:
    virtual ~ITrackeditProject() = default;

    virtual std::vector<TrackId> trackIdList() const = 0;
    virtual std::vector<Track> trackList() const = 0;
    virtual Clip clip(const ClipKey& key) const = 0;
    virtual muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const = 0;
    virtual std::vector<int64_t> groupsIdsList() const = 0;
    virtual std::optional<std::string> trackName(const TrackId& trackId) const = 0;

    virtual void reload() = 0;

    virtual void notifyAboutTrackAdded(const Track& track) = 0;
    virtual void notifyAboutTrackChanged(const Track& track) = 0;
    virtual void notifyAboutTrackRemoved(const Track& track) = 0;
    virtual void notifyAboutTrackInserted(const Track& track, int pos) = 0;
    virtual void notifyAboutTrackMoved(const Track& track, int pos) = 0;

    virtual void notifyAboutClipChanged(const Clip& clip) = 0;
    virtual void notifyAboutClipAdded(const Clip& clip) = 0;
    virtual void notifyAboutClipRemoved(const Clip& clip) = 0;

    virtual TimeSignature timeSignature() const = 0;
    virtual void setTimeSignature(const TimeSignature& timeSignature) = 0;
    virtual muse::async::Channel<TimeSignature> timeSignatureChanged() const = 0;

    virtual muse::async::Channel<std::vector<au::trackedit::Track> > tracksChanged() const = 0;
    virtual muse::async::Channel<trackedit::Track> trackAdded() const = 0;
    virtual muse::async::Channel<trackedit::Track> trackChanged() const = 0;
    virtual muse::async::Channel<trackedit::Track> trackRemoved() const = 0;
    virtual muse::async::Channel<trackedit::Track, int> trackInserted() const = 0;
    virtual muse::async::Channel<trackedit::Track, int> trackMoved() const = 0;

    virtual secs_t totalTime() const = 0;
};

using ITrackeditProjectPtr = std::shared_ptr<ITrackeditProject>;

class ITrackeditProjectCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditProjectCreator)
public:
    virtual ~ITrackeditProjectCreator() = default;

    virtual ITrackeditProjectPtr create(const std::shared_ptr<au::au3::IAu3Project>& au3project) const = 0;
};
}
