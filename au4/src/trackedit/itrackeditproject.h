/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "modularity/imoduleinterface.h"

#include "global/async/notifylist.h"
#include "global/async/channel.h"

#include "trackedittypes.h"
#include "dom/track.h"

namespace au::trackedit {
class ITrackeditProject
{
public:
    virtual ~ITrackeditProject() = default;

    virtual std::vector<TrackId> trackIdList() const = 0;
    virtual muse::async::NotifyList<Track> trackList() const = 0;
    virtual Clip clip(const ClipKey& key) const = 0;
    virtual muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const = 0;

    virtual void onClipChanged(const Clip& clip) = 0;
    virtual void onClipAdded(const Clip& clip) = 0;
    virtual void onClipRemoved(const Clip& clip) = 0;

    virtual TimeSignature timeSignature() const = 0;
    virtual void setTimeSignature(const TimeSignature& timeSignature) = 0;
    virtual muse::async::Channel<TimeSignature> timeSignatureChanged() const = 0;

    //! TODO Need remove from here to separated service
    virtual void pushHistoryState(const std::string& longDescription, const std::string& shortDescription) = 0;
};

using ITrackeditProjectPtr = std::shared_ptr<ITrackeditProject>;

class ITrackeditProjectCreator : MODULE_EXPORT_INTERFACE
{
    INTERFACE_ID(ITrackeditProjectCreator)
public:
    virtual ~ITrackeditProjectCreator() = default;

    virtual ITrackeditProjectPtr create() const = 0;
};
}
