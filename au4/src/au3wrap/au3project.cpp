#include "au3project.h"

#include "global/defer.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"
#include "libraries/lib-wave-track/WaveTrack.h"
#include "libraries/lib-wave-track/WaveClip.h"

//! HACK
//! Static variable is not initialized
//! static SampleBlockFactory::Factory::Scope scope{ []( AudacityProject &project )
//! so, to fix it, included this file here
#include "libraries/lib-project-file-io/SqliteSampleBlock.cpp"

#include "wxtypes_convert.h"
#include "internal/domconverter.h"
#include "internal/domaccessor.h"

#include "log.h"

using namespace au::au3;

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

bool Au3Project::save(const muse::io::path_t& filePath, const bool fromSaveAs)
{
    //! TODO AU4
    // auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    // std::string sstr = filePath.toStdString();
    // FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());
    // TrackList& tracks = TrackList::Get(m_data->projectRef());
    // bool success = projectFileIO.SaveProject(fileName, nullptr /* m_lastSavedTracks*/);
    return false;
}

void Au3Project::close()
{
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
    int index = -1;
    for (const std::shared_ptr<const WaveClip>& interval : waveTrack->Intervals()) {
        ++index;
        au::processing::Clip clip = DomConverter::clip(waveTrack, interval.get(), index);
        clips.push_back(std::move(clip));
    }

    return clips;
}

uintptr_t Au3Project::au3ProjectPtr() const
{
    return reinterpret_cast<uintptr_t>(m_data->project.get());
}
