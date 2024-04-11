#include "audacity3project.h"

#include "libraries/lib-project/Project.h"
#include "libraries/lib-project-file-io/ProjectFileIO.h"

//! HACK
//! Static variable is not initialized
//! static SampleBlockFactory::Factory::Scope scope{ []( AudacityProject &project )
//! so, to fix it, included this file here
#include "libraries/lib-project-file-io/SqliteSampleBlock.cpp"

#include "wxtypes_convert.h"

#include "log.h"

using namespace au::au3;

struct au::au3::Audacity3ProjectData
{
    std::shared_ptr<AudacityProject> project;

    AudacityProject& projectRef() { return *project.get(); }
};

Audacity3Project::Audacity3Project()
{
    m_data = std::make_shared<Audacity3ProjectData>();
}

std::shared_ptr<Audacity3Project> Audacity3Project::create()
{
    std::shared_ptr<Audacity3Project> p = std::make_shared<Audacity3Project>();
    p->m_data->project = AudacityProject::Create();
    return p;
}

bool Audacity3Project::load(const muse::io::path_t& filePath)
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    std::string sstr = filePath.toStdString();
    FilePath fileName = wxString::FromUTF8(sstr.c_str(), sstr.size());
    auto conn = projectFileIO.LoadProject(fileName, true);
    const bool bParseSuccess = conn.has_value();
    if (!bParseSuccess) {
        LOGE() << "failed load project: " << filePath;
    }

    conn->Commit();

    return bParseSuccess;
}

void Audacity3Project::close()
{
    auto& projectFileIO = ProjectFileIO::Get(m_data->projectRef());
    projectFileIO.CloseProject();
}

std::string Audacity3Project::title() const
{
    if (!m_data->project) {
        return std::string();
    }

    return wxToStdSting(m_data->project->GetProjectName());
}

au::processing::TrackList Audacity3Project::tracks() const
{
    au::processing::TrackList au4tracks;
    TrackList& tracks = TrackList::Get(m_data->projectRef());

    for (const Track* t : tracks) {
        TrackId id = t->GetId();
        au::processing::Track au4t;
        au4t.id = muse::ID(*(reinterpret_cast<long*>(&id)));
        au4t.title = wxToSting(t->GetName());

        au4tracks.push_back(std::move(au4t));
    }

    return au4tracks;
}
