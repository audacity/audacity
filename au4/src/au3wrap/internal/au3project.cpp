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
#include "TempoChange.h"

//! HACK
//! Static variable is not initialized
//! static SampleBlockFactory::Factory::Scope scope{ []( AudacityProject &project )
//! so, to fix it, included this file here
#include "libraries/lib-project-file-io/SqliteSampleBlock.cpp"

#include "wxtypes_convert.h"

#include "log.h"

using namespace au::au3;

std::shared_ptr<IAu3Project> Au3ProjectCreator::create() const
{
    return Au3Project::create();
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

    std::optional<ProjectFileIO::TentativeConnection> conn;

    try {
        conn.emplace(projectFileIO.LoadProject(fileName, true).value());
    } catch (AudacityException&) {
        LOGE() << "failed load project: " << filePath << ", exception received";
        return false;
    }

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
        //! TODO AU4: adjust to actual project tempo
        DoProjectTempoChange(*pTrack, 120);
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

uintptr_t Au3Project::au3ProjectPtr() const
{
    return reinterpret_cast<uintptr_t>(m_data->project.get());
}
