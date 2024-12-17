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
#include "domconverter.h"
#include "TempoChange.h"

//! HACK
//! Static variable is not initialized
//! static SampleBlockFactory::Factory::Scope scope{ []( AudacityProject &project )
//! so, to fix it, included this file here
#include "libraries/lib-project-file-io/SqliteSampleBlock.cpp"

#include "wxtypes_convert.h"
#include "../au3types.h"

#include "log.h"

using namespace au::au3;

std::shared_ptr<IAu3Project> Au3ProjectCreator::create() const
{
    return std::make_shared<Au3ProjectAccessor>();
}

struct au::au3::Au3ProjectData
{
    std::shared_ptr<Au3Project> project;

    Au3Project& projectRef() { return *project.get(); }
};

Au3ProjectAccessor::Au3ProjectAccessor()
    : m_data(std::make_shared<Au3ProjectData>())
{
    m_data->project = Au3Project::Create();
    mTrackListSubstription = Au3TrackList::Get(m_data->projectRef()).Subscribe([this](const TrackListEvent& event)
    {
        if (event.mType == TrackListEvent::ADDITION) {
            const auto tempo = ProjectTimeSignature::Get(m_data->projectRef()).GetTempo();
            if (const auto track = event.mpTrack.lock()) {
                DoProjectTempoChange(*track, tempo);
            }
        }
    });
}

void Au3ProjectAccessor::open()
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    projectFileIO.OpenProject();
}

bool Au3ProjectAccessor::load(const muse::io::path_t& filePath)
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    std::string sstr = filePath.toStdString();
    FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());

    std::optional<ProjectFileIO::TentativeConnection> conn;

    try {
        conn.emplace(projectFileIO.LoadProject(fileName, false /*ignoreAutosave*/).value());
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
    Au3TrackList& tracks = Au3TrackList::Get(m_data->projectRef());
    for (auto pTrack : tracks) {
        pTrack->LinkConsistencyFix();
    }

    m_hasSavedVersion = true;

    return true;
}

bool Au3ProjectAccessor::save(const muse::io::path_t& filePath)
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    Au3TrackList& tracks = Au3TrackList::Get(m_data->projectRef());
    auto result = projectFileIO.SaveProject(wxFromString(filePath.toString()), &tracks);
    if (result) {
        UndoManager::Get(m_data->projectRef()).StateSaved();
        // Project is now saved on disk - this isn't a new project anymore.
        m_hasSavedVersion = true;
    }
    return result;
}

void Au3ProjectAccessor::close()
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    projectFileIO.CloseProject();
    m_hasSavedVersion = false;
}

std::string Au3ProjectAccessor::title() const
{
    if (!m_data->project) {
        return std::string();
    }

    return wxToStdSting(m_data->project->GetProjectName());
}

bool Au3ProjectAccessor::hasSavedVersion() const
{
    return m_hasSavedVersion;
}

uintptr_t Au3ProjectAccessor::au3ProjectPtr() const
{
    return reinterpret_cast<uintptr_t>(m_data->project.get());
}
