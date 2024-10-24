#include "au3trackeditproject.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-numeric-formats/ProjectTimeSignature.h"
#include "libraries/lib-project-history/ProjectHistory.h"
#include "libraries/lib-stretching-sequence/TempoChange.h"

#include "au3wrap/iau3project.h"
#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "log.h"
#include "UndoManager.h"

using namespace muse;
using namespace au::trackedit;
using namespace au::au3;

struct Au3TrackeditProject::Au3Impl
{
    Au3Project* prj = nullptr;
    Au3TrackList* trackList = nullptr;

    // events
    Observer::Subscription tracksSubc;
    bool trackReplacing = false;
};

Au3TrackeditProject::Au3TrackeditProject(const std::shared_ptr<IAu3Project>& au3project)
{
    m_impl = std::make_shared<Au3Impl>();
    m_impl->prj = reinterpret_cast<Au3Project*>(au3project->au3ProjectPtr());
    m_impl->trackList = &Au3TrackList::Get(*m_impl->prj);
    m_impl->tracksSubc = m_impl->trackList->Subscribe([this](const TrackListEvent& e) {
        onTrackListEvent(e);
    });
}

Au3TrackeditProject::~Au3TrackeditProject()
{
    m_impl->tracksSubc.Reset();
}

std::vector<au::trackedit::TrackId> Au3TrackeditProject::trackIdList() const
{
    std::vector<au::trackedit::TrackId> au4trackIds;

    for (const Au3Track* t : *m_impl->trackList) {
        au4trackIds.push_back(DomConverter::trackId(t->GetId()));
    }

    return au4trackIds;
}

muse::async::NotifyList<au::trackedit::Track> Au3TrackeditProject::trackList() const
{
    muse::async::NotifyList<Track> au4tracks;

    for (const Au3Track* t : *m_impl->trackList) {
        Track au4t = DomConverter::track(t);
        au4tracks.push_back(std::move(au4t));
    }

    au4tracks.setNotify(m_tracksChanged.notify());

    return au4tracks;
}

static std::string eventTypeToString(const TrackListEvent& e)
{
    switch (e.mType) {
    case TrackListEvent::SELECTION_CHANGE: return "SELECTION_CHANGE";
    case TrackListEvent::TRACK_DATA_CHANGE: return "TRACK_DATA_CHANGE";
    case TrackListEvent::PERMUTED: return "PERMUTED";
    case TrackListEvent::RESIZING: return "RESIZING";
    case TrackListEvent::ADDITION: return "ADDITION";
    case TrackListEvent::DELETION: return e.mExtra ? "REPLACING" : "DELETION";
    }
}

void Au3TrackeditProject::onTrackListEvent(const TrackListEvent& e)
{
    TrackId trackId = -1;
    auto track = e.mpTrack.lock();
    if (track) {
        trackId = DomConverter::trackId(track->GetId());
    }
    LOGD() << "trackId: " << trackId << ", type: " << eventTypeToString(e);

    switch (e.mType) {
    case TrackListEvent::DELETION: {
        if (e.mExtra == 1) {
            m_impl->trackReplacing = true;
        }
    } break;
    case TrackListEvent::ADDITION: {
        if (m_impl->trackReplacing) {
            onTrackDataChanged(trackId);
            m_impl->trackReplacing = false;
        }

        const auto tempo = ProjectTimeSignature::Get(*m_impl->prj).GetTempo();
        if (const auto track = e.mpTrack.lock()) {
            DoProjectTempoChange(*track, tempo);
        }
    } break;
    default:
        break;
    }
}

void Au3TrackeditProject::onTrackDataChanged(const TrackId& trackId)
{
    auto it = m_clipsChanged.find(trackId);
    if (it != m_clipsChanged.end()) {
        it->second.changed();
    }
}

muse::async::NotifyList<au::trackedit::Clip> Au3TrackeditProject::clipList(const au::trackedit::TrackId& trackId) const
{
    const Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(*m_impl->prj, Au3TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return muse::async::NotifyList<au::trackedit::Clip>();
    }

    muse::async::NotifyList<au::trackedit::Clip> clips;
    for (const std::shared_ptr<const Au3WaveClip>& interval : waveTrack->Intervals()) {
        au::trackedit::Clip clip = DomConverter::clip(waveTrack, interval.get());
        clips.push_back(std::move(clip));
    }

    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[trackId];
    clips.setNotify(notifier.notify());

    return clips;
}

void Au3TrackeditProject::reload()
{
    m_tracksChanged.changed();
}

void Au3TrackeditProject::onTrackAdded(const Track& track)
{
    m_tracksChanged.itemAdded(track);
}

void Au3TrackeditProject::onTrackChanged(const Track& track)
{
    m_tracksChanged.itemChanged(track);
}

void Au3TrackeditProject::onTrackRemoved(const Track& track)
{
    m_tracksChanged.itemRemoved(track);
}

au::trackedit::Clip Au3TrackeditProject::clip(const ClipKey& key) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(*m_impl->prj, Au3TrackId(key.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return Clip();
    }

    std::shared_ptr<Au3WaveClip> au3Clip = DomAccessor::findWaveClip(waveTrack, key.clipId);
    IF_ASSERT_FAILED(au3Clip) {
        return Clip();
    }

    return DomConverter::clip(waveTrack, au3Clip.get());
}

void Au3TrackeditProject::onClipChanged(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[clip.key.trackId];
    notifier.itemChanged(clip);
}

void Au3TrackeditProject::onClipRemoved(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[clip.key.trackId];
    notifier.itemRemoved(clip);
}

void Au3TrackeditProject::onClipAdded(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifer = m_clipsChanged[clip.key.trackId];
    notifer.itemAdded(clip);
}

au::trackedit::TimeSignature Au3TrackeditProject::timeSignature() const
{
    trackedit::TimeSignature result;

    ProjectTimeSignature& timeSig = ProjectTimeSignature::Get(*m_impl->prj);
    result.tempo = timeSig.GetTempo();
    result.lower = timeSig.GetLowerTimeSignature();
    result.upper = timeSig.GetUpperTimeSignature();

    return result;
}

void Au3TrackeditProject::setTimeSignature(const trackedit::TimeSignature& timeSignature)
{
    ProjectTimeSignature& timeSig = ProjectTimeSignature::Get(*m_impl->prj);
    timeSig.SetTempo(timeSignature.tempo);
    timeSig.SetUpperTimeSignature(timeSignature.upper);
    timeSig.SetLowerTimeSignature(timeSignature.lower);

    m_timeSignatureChanged.send(timeSignature);
}

muse::async::Channel<au::trackedit::TimeSignature> Au3TrackeditProject::timeSignatureChanged() const
{
    return m_timeSignatureChanged;
}

secs_t Au3TrackeditProject::totalTime() const
{
    return m_impl->trackList->GetEndTime();
}

ITrackeditProjectPtr Au3TrackeditProjectCreator::create(const std::shared_ptr<IAu3Project>& au3project) const
{
    return std::make_shared<Au3TrackeditProject>(au3project);
}
