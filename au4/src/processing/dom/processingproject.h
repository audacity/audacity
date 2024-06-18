#pragma once

#include <memory>

#include "async/notifylist.h"
#include "async/channel.h"

#include "track.h"
#include "../processingtypes.h"

namespace au::au3 {
class IAu3Project;
}

namespace au::processing {
//! NOTE See description of Audacity4Project
class ProcessingProject
{
public:
    ProcessingProject();

    void setAudacity3Project(std::shared_ptr<au::au3::IAu3Project> au3);
    const std::shared_ptr<au::au3::IAu3Project>& audacity3Project() const { return m_au3; }

    std::vector<TrackId> trackIdList() const;
    muse::async::NotifyList<Track> trackList() const;
    muse::async::NotifyList<Clip> clipList(const TrackId& trackId) const;

    void onClipChanged(const Clip& clip);

    processing::TimeSignature timeSignature() const;
    muse::async::Channel<processing::TimeSignature> timeSignatureChanged() const;

    //! NOTE Just for debug
    void dump();

private:

    std::shared_ptr<au::au3::IAu3Project> m_au3;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip>> m_clipsChanged;
};

using ProcessingProjectPtr = std::shared_ptr<ProcessingProject>;
}
