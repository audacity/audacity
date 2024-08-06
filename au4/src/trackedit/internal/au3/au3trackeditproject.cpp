#include "au3trackeditproject.h"

#include "libraries/lib-track/Track.h"
#include "libraries/lib-numeric-formats/ProjectTimeSignature.h"
#include "libraries/lib-project-history/ProjectHistory.h"

#include "au3wrap/internal/domconverter.h"
#include "au3wrap/internal/domaccessor.h"
#include "au3wrap/internal/wxtypes_convert.h"

using namespace muse;
using namespace au::trackedit;
using namespace au::au3;

AudacityProject* Au3TrackeditProject::au3ProjectPtr() const
{
    AudacityProject* project = reinterpret_cast<AudacityProject*>(globalContext()->currentProject()->au3ProjectPtr());
    return project;
}

static au::trackedit::TrackType trackType(const ::Track* track)
{
    switch (track->NChannels()) {
    case 0:
        return au::trackedit::TrackType::Label;
    case 1:
        return au::trackedit::TrackType::Mono;
    case 2:
        return au::trackedit::TrackType::Stereo;
    default:
        break;
    }

    return au::trackedit::TrackType::Undefined;
}

std::vector<au::trackedit::TrackId> Au3TrackeditProject::trackIdList() const
{
    std::vector<au::trackedit::TrackId> au4trackIds;

    ::TrackList& tracks = ::TrackList::Get(*au3ProjectPtr());

    for (const ::Track* t : tracks) {
        au4trackIds.push_back(DomConverter::trackId(t->GetId()));
    }

    return au4trackIds;
}

muse::async::NotifyList<au::trackedit::Track> Au3TrackeditProject::trackList() const
{
    muse::async::NotifyList<Track> au4tracks;
    au4tracks.setNotify(m_trackChangedNotifier.notify());

    ::TrackList& tracks = ::TrackList::Get(*au3ProjectPtr());

    for (const ::Track* t : tracks) {
        Track au4t;
        au4t.id = DomConverter::trackId(t->GetId());
        au4t.title = wxToSting(t->GetName());
        au4t.type = trackType(t);

        au4tracks.push_back(std::move(au4t));
    }

    return au4tracks;
}

muse::async::NotifyList<au::trackedit::Clip> Au3TrackeditProject::clipList(const au::trackedit::TrackId& trackId) const
{
    const WaveTrack* waveTrack = DomAccessor::findWaveTrack(*au3ProjectPtr(), ::TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return muse::async::NotifyList<au::trackedit::Clip>();
    }

    muse::async::NotifyList<au::trackedit::Clip> clips;
    int index = -1;
    for (const std::shared_ptr<const WaveClip>& interval : waveTrack->Intervals()) {
        ++index;
        au::trackedit::Clip clip = DomConverter::clip(waveTrack, interval.get(), index);
        clips.push_back(std::move(clip));
    }

    async::ChangedNotifier<Clip>& notifier = m_clipsChanged[trackId];
    clips.setNotify(notifier.notify());

    return clips;
}

Clip Au3TrackeditProject::clip(const ClipKey& key) const
{
    return clipList(key.trackId)[key.index];
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
    if (au3ProjectPtr()) {
        return trackedit::TimeSignature();
    }

    trackedit::TimeSignature result;

    ProjectTimeSignature& timeSig = ProjectTimeSignature::Get(*au3ProjectPtr());
    result.tempo = timeSig.GetTempo();
    result.lower = timeSig.GetLowerTimeSignature();
    result.upper = timeSig.GetUpperTimeSignature();

    return result;
}

void Au3TrackeditProject::setTimeSignature(const trackedit::TimeSignature& timeSignature)
{
    if (au3ProjectPtr()) {
        return;
    }

    ProjectTimeSignature& timeSig = ProjectTimeSignature::Get(*au3ProjectPtr());
    timeSig.SetTempo(timeSignature.tempo);
    timeSig.SetUpperTimeSignature(timeSignature.upper);
    timeSig.SetLowerTimeSignature(timeSignature.lower);

    m_timeSignatureChanged.send(timeSignature);
}

muse::async::Channel<au::trackedit::TimeSignature> Au3TrackeditProject::timeSignatureChanged() const
{
    return m_timeSignatureChanged;
}

void Au3TrackeditProject::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    auto project = reinterpret_cast<AudacityProject*>(au3ProjectPtr());
    ProjectHistory::Get(*project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} });
}

ITrackeditProjectPtr Au3TrackeditProjectCreator::create() const
{
    return std::make_shared<Au3TrackeditProject>();
}
