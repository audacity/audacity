/*
* Audacity: A Digital Audio Editor
*/
#include "au3project.h"

#include "global/defer.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"
#include "libraries/lib-numeric-formats/ProjectTimeSignature.h"

//! HACK
//! Static variable is not initialized
//! static SampleBlockFactory::Factory::Scope scope{ []( AudacityProject &project )
//! so, to fix it, included this file here
#include "libraries/lib-project-file-io/SqliteSampleBlock.cpp"

#include "wxtypes_convert.h"
#include "domconverter.h"
#include "domaccessor.h"

#include "log.h"

using namespace au::au3;

std::shared_ptr<IAu3Project> Au3ProjectCreator::create() const
{
    return Au3Project::create();
}

static au::processing::TrackType trackType(const Track* track)
{
    switch (track->NChannels()) {
    case 0:
        return au::processing::TrackType::Label;
    case 1:
        return au::processing::TrackType::Mono;
    case 2:
        return au::processing::TrackType::Stereo;
    default:
        break;
    }

    return au::processing::TrackType::Undefined;
}

struct au::au3::Au3ProjectData
{
    std::shared_ptr<AudacityProject> project;

    AudacityProject& projectRef() { return *project.get(); }
};

Au3Project::Au3Project()
{
    m_data = std::make_shared<Au3ProjectData>();
}

std::shared_ptr<Au3Project> Au3Project::create()
{
    std::shared_ptr<Au3Project> p = std::make_shared<Au3Project>();
    p->m_data->project = AudacityProject::Create();
    return p;
}

bool Au3Project::load(const muse::io::path_t& filePath)
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    std::string sstr = filePath.toStdString();
    FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());
    auto conn = projectFileIO.LoadProject(fileName, true);

    muse::Defer([&conn]() {
        conn->Commit();
    });

    const bool bParseSuccess = conn.has_value();
    if (!bParseSuccess) {
        LOGE() << "failed load project: " << filePath;
        return false;
    }

    //! TODO Look like, need doing all from method  ProjectFileManager::FixTracks
    //! and maybe what is done before this method (ProjectFileManager::ReadProjectFile)
    TrackList& tracks = TrackList::Get(m_data->projectRef());
    for (auto pTrack : tracks) {
        pTrack->LinkConsistencyFix();
    }

    return true;
}

bool Au3Project::save(const muse::io::path_t& filePath)
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    TrackList& tracks = TrackList::Get(m_data->projectRef());
    auto result = projectFileIO.SaveProject(wxFromString(filePath.toString()), &tracks);
    if (result) {
        UndoManager::Get(m_data->projectRef()).StateSaved();
    }
    return result;
}

void Au3Project::close()
{
    UndoManager::Get(m_data->projectRef()).ClearStates();

    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    projectFileIO.CloseProject();
}

std::string Au3Project::title() const
{
    if (!m_data->project) {
        return std::string();
    }

    return wxToStdSting(m_data->project->GetProjectName());
}

std::vector<au::processing::TrackId> Au3Project::trackIdList() const
{
    std::vector<au::processing::TrackId> au4trackIds;

    TrackList& tracks = TrackList::Get(m_data->projectRef());

    for (const Track* t : tracks) {
        au4trackIds.push_back(DomConverter::trackId(t->GetId()));
    }

    return au4trackIds;
}

muse::async::NotifyList<au::processing::Track> Au3Project::trackList() const
{
    muse::async::NotifyList<au::processing::Track> au4tracks;
    au4tracks.setNotify(m_trackChangedNotifier.notify());

    TrackList& tracks = TrackList::Get(m_data->projectRef());

    for (const Track* t : tracks) {
        au::processing::Track au4t;
        au4t.id = DomConverter::trackId(t->GetId());
        au4t.title = wxToSting(t->GetName());
        au4t.type = trackType(t);

        au4tracks.push_back(std::move(au4t));
    }

    return au4tracks;
}

muse::async::NotifyList<au::processing::Clip> Au3Project::clipList(const au::processing::TrackId& trackId) const
{
    const WaveTrack* waveTrack = DomAccessor::findWaveTrack(m_data->projectRef(), TrackId(trackId));
    IF_ASSERT_FAILED(waveTrack) {
        return muse::async::NotifyList<au::processing::Clip>();
    }

    muse::async::NotifyList<au::processing::Clip> clips;
    for (const std::shared_ptr<const WaveClip>& interval : waveTrack->Intervals()) {
        au::processing::Clip clip = DomConverter::clip(waveTrack, interval.get());
        clips.push_back(std::move(clip));
    }

    return clips;
}

au::processing::TimeSignature Au3Project::timeSignature() const
{
    if (!m_data->project) {
        return processing::TimeSignature();
    }

    processing::TimeSignature result;

    ProjectTimeSignature& timeSig = ProjectTimeSignature::Get(m_data->projectRef());
    result.tempo = timeSig.GetTempo();
    result.lower = timeSig.GetLowerTimeSignature();
    result.upper = timeSig.GetUpperTimeSignature();

    return result;
}

void Au3Project::setTimeSignature(const processing::TimeSignature& timeSignature)
{
    if (!m_data->project) {
        return;
    }

    ProjectTimeSignature& timeSig = ProjectTimeSignature::Get(m_data->projectRef());
    timeSig.SetTempo(timeSignature.tempo);
    timeSig.SetUpperTimeSignature(timeSignature.upper);
    timeSig.SetLowerTimeSignature(timeSignature.lower);

    m_timeSignatureChanged.send(timeSignature);
}

muse::async::Channel<au::processing::TimeSignature> Au3Project::timeSignatureChanged() const
{
    return m_timeSignatureChanged;
}

void Au3Project::pushHistoryState(const std::string& longDescription, const std::string& shortDescription)
{
    auto project = reinterpret_cast<AudacityProject*>(au3ProjectPtr());
    ProjectHistory::Get(*project).PushState(TranslatableString { longDescription, {} }, TranslatableString { shortDescription, {} });
}

uintptr_t Au3Project::au3ProjectPtr() const
{
    return reinterpret_cast<uintptr_t>(m_data->project.get());
}
