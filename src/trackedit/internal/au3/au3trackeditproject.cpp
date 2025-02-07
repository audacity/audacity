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
    Observer::Subscription projectTimeSignatureSubscription;
};

Au3TrackeditProject::Au3TrackeditProject(const std::shared_ptr<IAu3Project>& au3project)
{
    m_impl = std::make_shared<Au3Impl>();
    m_impl->prj = reinterpret_cast<Au3Project*>(au3project->au3ProjectPtr());
    m_impl->trackList = &Au3TrackList::Get(*m_impl->prj);
    m_impl->tracksSubc = m_impl->trackList->Subscribe([this](const TrackListEvent& e) {
        onTrackListEvent(e);
    });
    m_impl->projectTimeSignatureSubscription = ProjectTimeSignature::Get(*m_impl->prj).Subscribe(
        [this](const TimeSignatureChangedMessage& event) {
        onProjectTempoChange(
            event.newTempo);

        au::trackedit::TimeSignature trackeditTimeSignature = au::trackedit::TimeSignature { event.newTempo, event.newUpperTimeSignature, event.newLowerTimeSignature };
        m_timeSignatureChanged.send(trackeditTimeSignature);
    });
}

Au3TrackeditProject::~Au3TrackeditProject()
{
    m_impl->tracksSubc.Reset();
}

std::vector<int64_t> Au3TrackeditProject::groupsIdsList() const
{
    std::vector<int64_t> groupsList;

    for (const auto& trackId : trackIdList()) {
        Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(*m_impl->prj, Au3TrackId(trackId));
        IF_ASSERT_FAILED(waveTrack) {
            return {};
        }

        for (const auto& key : clipList(trackId)) {
            std::shared_ptr<Au3WaveClip> au3Clip = DomAccessor::findWaveClip(waveTrack, key.key.clipId);
            IF_ASSERT_FAILED(au3Clip) {
                return {};
            }

            int64_t groupId = au3Clip->GetGroupId();
            if (!muse::contains(groupsList, groupId)) {
                groupsList.push_back(groupId);
            }
        }
    }

    return groupsList;
}

std::vector<au::trackedit::TrackId> Au3TrackeditProject::trackIdList() const
{
    std::vector<au::trackedit::TrackId> au4trackIds;

    for (const Au3Track* t : *m_impl->trackList) {
        au4trackIds.push_back(t->GetId());
    }

    return au4trackIds;
}

std::vector<au::trackedit::Track> Au3TrackeditProject::trackList() const
{
    std::vector<au::trackedit::Track> au4tracks;

    for (const Au3Track* t : *m_impl->trackList) {
        Track au4t = DomConverter::track(t);
        au4tracks.push_back(std::move(au4t));
    }

    //TODO AU4: For now we filter out label tracks
    au4tracks.erase(std::remove_if(au4tracks.begin(), au4tracks.end(), [](const Track& t) {
        if (t.type == au::trackedit::TrackType::Label) {
            LOGW() << "Label tracks not implemented, so it will be filtered out.";
            return true;
        }
        return false;
    }), au4tracks.end());

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
    if (e.mType == TrackListEvent::UNDO_REDO_BEGIN || e.mType == TrackListEvent::UNDO_REDO_END) {
        return;
    }

    TrackId trackId = -1;
    auto track = e.mpTrack.lock();
    if (track) {
        trackId = track->GetId();
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

void Au3TrackeditProject::onProjectTempoChange(double newTempo)
{
    Au3TrackList& tracks = Au3TrackList::Get(*m_impl->prj);
    for (auto track : tracks) {
        DoProjectTempoChange(*track, newTempo);
        TrackId trackId = track->GetId();
        onTrackDataChanged(trackId);
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

std::optional<std::string> Au3TrackeditProject::trackName(const TrackId& trackId) const
{
    const Au3Track* au3Track = au::au3::DomAccessor::findTrack(*m_impl->prj, au::au3::Au3TrackId { trackId });
    if (au3Track == nullptr) {
        return std::nullopt;
    }
    return au::au3::DomConverter::track(au3Track).title.toStdString();
}

void Au3TrackeditProject::reload()
{
    m_tracksChanged.send(trackList());
}

void Au3TrackeditProject::notifyAboutTrackAdded(const Track& track)
{
    m_trackAdded.send(track);
}

void Au3TrackeditProject::notifyAboutTrackChanged(const Track& track)
{
    m_trackChanged.send(track);
}

void Au3TrackeditProject::notifyAboutTrackRemoved(const Track& track)
{
    m_trackRemoved.send(track);
}

void Au3TrackeditProject::notifyAboutTrackInserted(const Track& track, int pos)
{
    m_trackInserted.send(track, pos);
}

void Au3TrackeditProject::notifyAboutTrackMoved(const Track& track, int pos)
{
    return m_trackMoved.send(track, pos);
}

au::trackedit::Clip Au3TrackeditProject::clip(const ClipKey& key) const
{
    Au3WaveTrack* waveTrack = DomAccessor::findWaveTrack(*m_impl->prj, Au3TrackId(key.trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return Clip();
    }

    std::shared_ptr<Au3WaveClip> au3Clip = DomAccessor::findWaveClip(waveTrack, key.clipId);
    if (!au3Clip) {
        return Clip();
    }

    return DomConverter::clip(waveTrack, au3Clip.get());
}

void Au3TrackeditProject::notifyAboutClipChanged(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[clip.key.trackId];
    notifier.itemChanged(clip);
}

void Au3TrackeditProject::notifyAboutClipRemoved(const Clip& clip)
{
    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[clip.key.trackId];
    notifier.itemRemoved(clip);
}

void Au3TrackeditProject::notifyAboutClipAdded(const Clip& clip)
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

    std::string historyStateMessage;
    if (!muse::is_equal(timeSig.GetTempo(), timeSignature.tempo)) {
        historyStateMessage = "Tempo changed";
    }
    timeSig.SetTempo(timeSignature.tempo);

    if (!muse::is_equal(timeSig.GetUpperTimeSignature(), timeSignature.upper)) {
        historyStateMessage = "Upper time signature changed";
    }
    timeSig.SetUpperTimeSignature(timeSignature.upper);

    if (!muse::is_equal(timeSig.GetLowerTimeSignature(), timeSignature.lower)) {
        historyStateMessage = "Lower time signature changed";
    }
    timeSig.SetLowerTimeSignature(timeSignature.lower);

    projectHistory()->pushHistoryState(historyStateMessage, historyStateMessage, trackedit::UndoPushType::CONSOLIDATE);
}

muse::async::Channel<au::trackedit::TimeSignature> Au3TrackeditProject::timeSignatureChanged() const
{
    return m_timeSignatureChanged;
}

muse::async::Channel<std::vector<au::trackedit::Track> > Au3TrackeditProject::tracksChanged() const
{
    return m_tracksChanged;
}

muse::async::Channel<au::trackedit::Track> Au3TrackeditProject::trackAdded() const
{
    return m_trackAdded;
}

muse::async::Channel<au::trackedit::Track> Au3TrackeditProject::trackChanged() const
{
    return m_trackChanged;
}

muse::async::Channel<au::trackedit::Track> Au3TrackeditProject::trackRemoved() const
{
    return m_trackRemoved;
}

muse::async::Channel<au::trackedit::Track, int> Au3TrackeditProject::trackInserted() const
{
    return m_trackInserted;
}

muse::async::Channel<au::trackedit::Track, int> Au3TrackeditProject::trackMoved() const
{
    return m_trackMoved;
}

secs_t Au3TrackeditProject::totalTime() const
{
    return m_impl->trackList->GetEndTime();
}

ITrackeditProjectPtr Au3TrackeditProjectCreator::create(const std::shared_ptr<IAu3Project>& au3project) const
{
    return std::make_shared<Au3TrackeditProject>(au3project);
}

TimeSignatureRestorer::TimeSignatureRestorer(AudacityProject& project)
    : mTempo{ProjectTimeSignature::Get(project).GetTempo()}
    , mUpper{ProjectTimeSignature::Get(project).GetUpperTimeSignature()}
    , mLower{ProjectTimeSignature::Get(project).GetLowerTimeSignature()}
{
}

void TimeSignatureRestorer::RestoreUndoRedoState(AudacityProject& project)
{
    auto& timeSignature = ProjectTimeSignature::Get(project);

    timeSignature.SetTempo(mTempo);
    timeSignature.SetUpperTimeSignature(mUpper);
    timeSignature.SetLowerTimeSignature(mLower);
}

void TimeSignatureRestorer::reg()
{
    static UndoRedoExtensionRegistry::Entry sEntry {
        [](AudacityProject& project) -> std::shared_ptr<UndoStateExtension>
        { return std::make_shared<TimeSignatureRestorer>(project); }
    };
}
