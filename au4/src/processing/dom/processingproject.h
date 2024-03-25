#ifndef AU_PROСESSING_PROCESSINGPROJECT_H
#define AU_PROСESSING_PROCESSINGPROJECT_H

#include "track.h"

namespace au::processing {
class ProcessingProject
{
public:
    ProcessingProject();

    const TrackList& trackList() const { return m_trackList; }

private:
    TrackList m_trackList;
};
}

#endif // AU_PROСESSING_PROCESSINGPROJECT_H
