#ifndef AU_PROСESSING_PROCESSINGPROJECT_H
#define AU_PROСESSING_PROCESSINGPROJECT_H

#include <memory>

#include "async/notifylist.h"

#include "track.h"

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

    //! NOTE Just for debug
    void dump();

private:

    std::shared_ptr<au::au3::IAu3Project> m_au3;

    mutable std::map<TrackId, muse::async::ChangedNotifier<Clip>> m_clipsChanged;
};

using ProcessingProjectPtr = std::shared_ptr<ProcessingProject>;
}

#endif // AU_PROСESSING_PROCESSINGPROJECT_H
