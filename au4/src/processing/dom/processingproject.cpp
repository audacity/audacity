#include "processingproject.h"

#include "au3wrap/audacity3project.h"

#include "log.h"

using namespace au::processing;

ProcessingProject::ProcessingProject() {}

void ProcessingProject::setAudacity3Project(std::shared_ptr<au::au3::Audacity3Project> au3)
{
    m_au3 = au3;
}

muse::async::NotifyList<Track> ProcessingProject::trackList() const
{
    return m_au3->trackList();
}

muse::async::NotifyList<Clip> ProcessingProject::clipList(const TrackId& trackId) const
{
    return m_au3->clipList(trackId);
}

void ProcessingProject::dump()
{
    muse::async::NotifyList<Track> tracks = trackList();
    LOGDA() << "tracks: " << tracks.size();
    for (const Track& t : tracks) {
        LOGDA() << "id: " << t.id << ", title: " << t.title;
    }
}
