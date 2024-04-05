#ifndef AU_PROСESSING_PROCESSINGPROJECT_H
#define AU_PROСESSING_PROCESSINGPROJECT_H

#include <memory>

#include "track.h"

namespace au::au3 {
class Audacity3Project;
}

namespace au::processing {
//! NOTE See description of Audacity4Project
class ProcessingProject
{
public:
    ProcessingProject();

    void setAudacity3Project(std::shared_ptr<au::au3::Audacity3Project> au3);

    TrackList trackList() const;

    //! NOTE Just for debug
    void dump();

private:

    std::shared_ptr<au::au3::Audacity3Project> m_au3;
};

using ProcessingProjectPtr = std::shared_ptr<ProcessingProject>;
}

#endif // AU_PROСESSING_PROCESSINGPROJECT_H
