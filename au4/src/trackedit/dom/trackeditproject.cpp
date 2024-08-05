#include "trackeditproject.h"

#include "au3wrap/iau3project.h"

#include "log.h"

using namespace muse;
using namespace au::trackedit;

TrackeditProject::TrackeditProject() {}

void TrackeditProject::setAudacity3Project(std::shared_ptr<au::au3::IAu3Project> au3)
{
    m_au3 = au3;
}

std::vector<TrackId> TrackeditProject::trackIdList() const
{
    return m_au3->trackIdList();
}

async::NotifyList<Track> TrackeditProject::trackList() const
{
    return m_au3->trackList();
}

Clip TrackeditProject::clip(const ClipKey& key) const
{
    return m_au3->clipList(key.trackId)[key.index];
}

async::NotifyList<Clip> TrackeditProject::clipList(const TrackId& trackId) const
{
    async::NotifyList<Clip> clips = m_au3->clipList(trackId);
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[trackId];
    clips.setNotify(notifier.notify());
    return clips;
}

void TrackeditProject::onClipChanged(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[clip.key.trackId];
    notifier.itemChanged(clip);
}

void TrackeditProject::onClipRemoved(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[clip.key.trackId];
    notifier.itemRemoved(clip);
}

void TrackeditProject::onClipAdded(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifer = m_clipsChanged[clip.key.trackId];
    notifer.itemAdded(clip);
}

TimeSignature TrackeditProject::timeSignature() const
{
    return m_au3->timeSignature();
}

void TrackeditProject::setTimeSignature(const TimeSignature& timeSignature)
{
    m_au3->setTimeSignature(timeSignature);
}

muse::async::Channel<TimeSignature> TrackeditProject::timeSignatureChanged() const
{
    return m_au3->timeSignatureChanged();
}

void TrackeditProject::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    m_au3->pushHistoryState(longDescription, shortDescription);
}

void TrackeditProject::dump()
{
    async::NotifyList<Track> tracks = trackList();
    LOGDA() << "tracks: " << tracks.size();
    for (const Track& t : tracks) {
        LOGDA() << "id: " << t.id << ", title: " << t.title;
    }
}
