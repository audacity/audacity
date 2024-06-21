#include "processingproject.h"

#include "au3wrap/iau3project.h"

#include "log.h"

using namespace muse;
using namespace au::processing;

ProcessingProject::ProcessingProject() {}

void ProcessingProject::setAudacity3Project(std::shared_ptr<au::au3::IAu3Project> au3)
{
    m_au3 = au3;
}

std::vector<TrackId> ProcessingProject::trackIdList() const
{
    return m_au3->trackIdList();
}

async::NotifyList<Track> ProcessingProject::trackList() const
{
    return m_au3->trackList();
}

async::NotifyList<Clip> ProcessingProject::clipList(const TrackId& trackId) const
{
    async::NotifyList<Clip> clips = m_au3->clipList(trackId);
    async::ChangedNotifier<Clip>& notifer = m_clipsChanged[trackId];
    clips.setNotify(notifer.notify());
    return clips;
}

void ProcessingProject::onClipChanged(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifer = m_clipsChanged[clip.key.trackId];
    notifer.itemChanged(clip);
}

TimeSignature ProcessingProject::timeSignature() const
{
    return m_au3->timeSignature();
}

void ProcessingProject::setTimeSignature(const TimeSignature& timeSignature)
{
    m_au3->setTimeSignature(timeSignature);
}

muse::async::Channel<TimeSignature> ProcessingProject::timeSignatureChanged() const
{
    return m_au3->timeSignatureChanged();
}

void ProcessingProject::dump()
{
    async::NotifyList<Track> tracks = trackList();
    LOGDA() << "tracks: " << tracks.size();
    for (const Track& t : tracks) {
        LOGDA() << "id: " << t.id << ", title: " << t.title;
    }
}
