#ifndef AU_PROСESSING_PROCESSINGPROJECT_H
#define AU_PROСESSING_PROCESSINGPROJECT_H

#include <memory>

#include "track.h"

namespace au::processing {
class ProcessingProject
{
public:
    ProcessingProject();

    const TrackList& trackList() const { return m_trackList; }
    void setTrackList(const TrackList& l) { m_trackList = l; }

private:
    TrackList m_trackList;
};

using ProcessingProjectPtr = std::shared_ptr<ProcessingProject>;
}

#endif // AU_PROСESSING_PROCESSINGPROJECT_H
