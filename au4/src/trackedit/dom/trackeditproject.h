/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include <memory>

#include "async/notifylist.h"
#include "async/channel.h"

#include "track.h"
#include "clip.h"
#include "../trackedittypes.h"

namespace au::au3 {
class IAu3Project;
}

namespace au::trackedit {
//! NOTE See description of Audacity4Project
class TrackeditProject
{
public:
    TrackeditProject();

    void setAudacity3Project(std::shared_ptr<au::au3::IAu3Project> au3);
    const std::shared_ptr<au::au3::IAu3Project>& audacity3Project() const { return m_au3; }

    std::vector<TrackId> trackIdList() const;
    muse::async::NotifyList<Track> trackList() const;
    Clip clip(const ClipKey& key) const;
    muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const;

    void onClipChanged(const Clip& clip);
    void onClipAdded(const Clip& clip);
    void onClipRemoved(const Clip& clip);

    trackedit::TimeSignature timeSignature() const;
    void setTimeSignature(const trackedit::TimeSignature& timeSignature);
    muse::async::Channel<trackedit::TimeSignature> timeSignatureChanged() const;

    void pushHistoryState(const std::string& longDescription, const std::string& shortDescription);

    //! NOTE Just for debug
    void dump();

private:

    std::shared_ptr<au::au3::IAu3Project> m_au3;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip>> m_clipsChanged;
};

using TrackeditProjectPtr = std::shared_ptr<TrackeditProject>;
}
